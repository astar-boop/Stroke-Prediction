from __future__ import annotations

import json
import mimetypes
import os
import subprocess
import sys
import threading
import time
import uuid
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, urlparse


APP_DIR = Path(__file__).resolve().parent
ROOT_DIR = APP_DIR.parent
STATIC_DIR = APP_DIR / "static"
BRIDGE = APP_DIR / "predict_bridge.R"
SHAP_SUMMARY_CACHE: dict[str, dict] = {}
PREDICT_RESULT_CACHE: dict[str, dict] = {}
PREDICT_JOBS: dict[str, dict] = {}
PREDICT_LOCK = threading.Lock()
MAX_JSON_BYTES = 1_000_000


class ClientInputError(ValueError):
    pass


def run_bridge(action: str, payload: dict | None = None) -> dict:
    body = json.dumps(payload or {}, ensure_ascii=False)
    proc = subprocess.run(
        ["Rscript", str(BRIDGE), action],
        input=body,
        text=True,
        cwd=str(ROOT_DIR),
        capture_output=True,
        timeout=180,
        env={**os.environ, "SU2026_WEB_DIR": str(APP_DIR)},
    )
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr.strip() or proc.stdout.strip() or "R bridge failed")
    try:
        return json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        raise RuntimeError(f"Invalid bridge JSON: {proc.stdout[:1000]}") from exc


def prune_predict_state() -> None:
    cutoff = time.time() - 900
    with PREDICT_LOCK:
        for store in (PREDICT_RESULT_CACHE, PREDICT_JOBS):
            for key, item in list(store.items()):
                created = item.get("created_at", 0) if isinstance(item, dict) else 0
                if created and created < cutoff:
                    store.pop(key, None)


def run_predict_job(request_id: str) -> None:
    with PREDICT_LOCK:
        job = PREDICT_JOBS.get(request_id)
        if not job:
            return
        payload = job["payload"]
        job["status"] = "running"
    try:
        result = run_bridge("predict", payload)
        with PREDICT_LOCK:
            PREDICT_RESULT_CACHE[request_id] = {"created_at": time.time(), "result": result}
            PREDICT_JOBS[request_id].update(status="done", result=result)
    except Exception as exc:
        with PREDICT_LOCK:
            PREDICT_JOBS[request_id].update(status="error", error=str(exc))


def start_predict_job(payload: dict) -> str:
    request_id = str(payload.get("request_id") or uuid.uuid4().hex)
    payload["request_id"] = request_id
    prune_predict_state()
    with PREDICT_LOCK:
        existing = PREDICT_JOBS.get(request_id)
        if existing and existing.get("status") in {"queued", "running", "done"}:
            return request_id
        PREDICT_JOBS[request_id] = {
            "created_at": time.time(),
            "status": "queued",
            "payload": payload,
        }
    thread = threading.Thread(target=run_predict_job, args=(request_id,), daemon=True)
    thread.start()
    return request_id


class Handler(BaseHTTPRequestHandler):
    server_version = "SU2026PredictionWeb/1.0"
    protocol_version = "HTTP/1.1"

    def log_message(self, fmt: str, *args) -> None:
        sys.stderr.write("%s - %s\n" % (self.address_string(), fmt % args))

    def send_json(self, data: dict, status: int = 200) -> None:
        raw = json.dumps(data, ensure_ascii=False).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(raw)))
        self.send_header("Cache-Control", "no-store")
        self.send_header("Connection", "close")
        self.end_headers()
        self.wfile.write(raw)
        self.wfile.flush()
        self.close_connection = True

    def send_static(self, path: Path) -> None:
        if not path.exists() or not path.is_file():
            self.send_error(404)
            return
        raw = path.read_bytes()
        ctype = mimetypes.guess_type(str(path))[0] or "application/octet-stream"
        self.send_response(200)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(raw)))
        self.send_header("Cache-Control", "no-store")
        self.send_header("Connection", "close")
        self.end_headers()
        self.wfile.write(raw)
        self.wfile.flush()
        self.close_connection = True

    def read_json(self) -> dict:
        try:
            n = int(self.headers.get("Content-Length", "0"))
        except ValueError as exc:
            raise ClientInputError("Content-Length non valido") from exc
        if n < 0 or n > MAX_JSON_BYTES:
            raise ClientInputError(f"Payload JSON troppo grande (massimo {MAX_JSON_BYTES} byte)")
        if n == 0:
            return {}
        try:
            payload = json.loads(self.rfile.read(n).decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError) as exc:
            raise ClientInputError("Payload JSON non valido") from exc
        if not isinstance(payload, dict):
            raise ClientInputError("Il payload JSON deve essere un oggetto")
        return payload

    def do_GET(self) -> None:
        parsed = urlparse(self.path)
        route = parsed.path
        try:
            if route == "/healthz":
                self.send_json({"status": "ok", "research_only": True})
            elif route == "/":
                self.send_static(STATIC_DIR / "index.html")
            elif route == "/api/metadata":
                self.send_json(run_bridge("metadata"))
            elif route == "/api/predict-result":
                request_id = parse_qs(parsed.query).get("request_id", [""])[0]
                with PREDICT_LOCK:
                    cached = PREDICT_RESULT_CACHE.get(request_id, {}).get("result") if request_id else None
                    job = PREDICT_JOBS.get(request_id) if request_id else None
                if cached is not None:
                    self.send_json(cached)
                elif job and job.get("status") in {"queued", "running"}:
                    self.send_json({"request_id": request_id, "status": job.get("status")})
                elif job and job.get("status") == "error":
                    self.send_json({"error": job.get("error", "Prediction job failed")}, status=500)
                else:
                    self.send_json({"error": "Prediction result not available"}, status=404)
            elif route.startswith("/static/"):
                target = (STATIC_DIR / route.removeprefix("/static/")).resolve()
                if STATIC_DIR not in target.parents and target != STATIC_DIR:
                    self.send_error(403)
                else:
                    self.send_static(target)
            else:
                self.send_error(404)
        except ClientInputError as exc:
            self.send_json({"error": str(exc)}, status=400)
        except Exception as exc:
            self.send_json({"error": str(exc)}, status=500)

    def do_HEAD(self) -> None:
        route = urlparse(self.path).path
        if route == "/":
            path = STATIC_DIR / "index.html"
        elif route.startswith("/static/"):
            path = (STATIC_DIR / route.removeprefix("/static/")).resolve()
            if STATIC_DIR not in path.parents and path != STATIC_DIR:
                self.send_error(403)
                return
        else:
            self.send_error(404)
            return
        if not path.exists() or not path.is_file():
            self.send_error(404)
            return
        ctype = mimetypes.guess_type(str(path))[0] or "application/octet-stream"
        self.send_response(200)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(path.stat().st_size))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()

    def do_POST(self) -> None:
        route = urlparse(self.path).path
        try:
            payload = self.read_json()
            if route == "/api/predict":
                request_id = str(payload.get("request_id", ""))
                if request_id and request_id in PREDICT_RESULT_CACHE:
                    self.send_json(PREDICT_RESULT_CACHE[request_id]["result"])
                    return
                result = run_bridge("predict", payload)
                if request_id:
                    PREDICT_RESULT_CACHE[request_id] = {"created_at": time.time(), "result": result}
                    if len(PREDICT_RESULT_CACHE) > 80:
                        for old_key in list(PREDICT_RESULT_CACHE.keys())[:20]:
                            PREDICT_RESULT_CACHE.pop(old_key, None)
                self.send_json(result)
            elif route == "/api/predict-job":
                request_id = start_predict_job(payload)
                self.send_json({"request_id": request_id, "status": "queued"})
            elif route == "/api/record":
                self.send_json(run_bridge("record", payload))
            elif route == "/api/shap-summary":
                task_id = str(payload.get("task_id", ""))
                cache_key = json.dumps(
                    {
                        "task_id": task_id,
                        "model_id": payload.get("model_id"),
                        "max_features": payload.get("max_features"),
                        "max_records": payload.get("max_records"),
                        "nsim": payload.get("nsim"),
                    },
                    sort_keys=True,
                )
                if cache_key not in SHAP_SUMMARY_CACHE:
                    SHAP_SUMMARY_CACHE[cache_key] = run_bridge("shap_summary", payload)
                self.send_json(SHAP_SUMMARY_CACHE[cache_key])
            else:
                self.send_error(404)
        except ClientInputError as exc:
            self.send_json({"error": str(exc)}, status=400)
        except Exception as exc:
            self.send_json({"error": str(exc)}, status=500)


def main() -> None:
    portable_artifact = os.environ.get("SU2026_PORTABLE_ARTIFACT", "").lower() in {"1", "true", "yes"}
    if not portable_artifact and not os.environ.get("SU2026_RUN_DIR") and not os.environ.get("SU2026_ANALYSIS_DATA"):
        raise RuntimeError(
            "Impostare SU2026_RUN_DIR oppure SU2026_ANALYSIS_DATA prima di avviare il server; "
            "non viene utilizzato alcun dataset di fallback."
        )
    metadata = run_bridge("metadata")
    if metadata.get("research_only") is not True or metadata.get("contains_patient_records") is not False:
        raise RuntimeError("Artefatto predittivo non conforme ai requisiti research-only/privacy")
    host = os.environ.get("HOST", "0.0.0.0" if os.environ.get("RENDER") else "127.0.0.1")
    port = int(os.environ.get("PORT") or (sys.argv[1] if len(sys.argv) > 1 else 3840))
    httpd = ThreadingHTTPServer((host, port), Handler)
    print(f"Listening on http://{host}:{port} (research only; run={metadata.get('source_run', 'unknown')})", flush=True)
    httpd.serve_forever()


if __name__ == "__main__":
    main()
