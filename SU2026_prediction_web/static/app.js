const state = {
  metadata: null,
  task: null,
  model: null,
  values: {},
  valueMemory: {},
  result: null,
  shapSummaryCache: new Map(),
  predictionRequestVersion: 0,
  shapSummaryRequestVersion: 0,
};

const ivtTimingVars = ["door_to_needle_min_recalc", "onset_to_needle_min_recalc"];
const evtTimingVars = [
  "onset_to_groin_min_recalc",
  "door_to_groin_min_recalc",
  "groin_to_tici_min_recalc",
  "onset_to_tici_min_recalc",
];

const els = {
  predictionForm: document.getElementById("predictionForm"),
  taskSelect: document.getElementById("taskSelect"),
  modelSelect: document.getElementById("modelSelect"),
  recordSelect: document.getElementById("recordSelect"),
  predictButton: document.getElementById("predictButton"),
  modelNotes: document.getElementById("modelNotes"),
  summaryPanel: document.getElementById("summaryPanel"),
  forcePlot: document.getElementById("forcePlot"),
  violinPlot: document.getElementById("violinPlot"),
  violinBadge: document.getElementById("violinBadge"),
  inputGrid: document.getElementById("inputGrid"),
  shapTable: document.getElementById("shapTable"),
  featureTable: document.getElementById("featureTable"),
  conditionalBadge: document.getElementById("conditionalBadge"),
  explanationBadge: document.getElementById("explanationBadge"),
  explanationNote: document.getElementById("explanationNote"),
  recordSummaryValue: document.getElementById("recordSummaryValue"),
  pathwaySummaryValue: document.getElementById("pathwaySummaryValue"),
  toast: document.getElementById("toast"),
};

function isYes(value) {
  const n = Number(String(value ?? "").replace(",", "."));
  return Number.isFinite(n) && n === 1;
}

function fmtPct(value) {
  return `${(100 * Number(value)).toFixed(1)}%`;
}

function fmtNum(value, digits = 3) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "";
  return n.toFixed(digits);
}

function fmtCompact(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "NA";
  return Number.isInteger(n) ? String(n) : n.toFixed(2).replace(/0+$/, "").replace(/\.$/, "");
}

function cleanLabel(name) {
  return String(name).replaceAll("_", " ").replace(/\b\w/g, (m) => m.toUpperCase());
}

const clinicalFeatureLabels = {
  eta: "Età",
  age_years_analysis: "Età",
  sesso_m_0_f_1: "Sesso",
  ischemico_0_emorragico_1_tia_2_altro_3: "Tipo di evento",
  ipertensione_arteriosa: "Ipertensione arteriosa",
  diabete: "Diabete",
  pregresso_ictus_tia: "Pregresso ictus/TIA",
  pregresso_ima: "Pregresso infarto miocardico",
  fumo_0_no_1_attivo_2_ex: "Abitudine al fumo",
  dislipidemia: "Dislipidemia",
  trombofilia: "Trombofilia",
  abuso_di_alcol: "Abuso di alcol",
  uso_di_sostanze_stupefacenti: "Uso di sostanze stupefacenti",
  fibrillazione_atriale: "Fibrillazione atriale",
  ateromasia_carotidea_50_percent: "Ateromasia carotidea ≥50%",
  ateromasia_vertebrale: "Ateromasia vertebrale",
  patologia_neoplastica: "Patologia neoplastica",
  utilizzo_contraccettivi_orali: "Uso di contraccettivi orali",
  familirita_per_ictus: "Familiarità per ictus",
  antiaggregante: "Terapia antiaggregante",
  anticoagulante: "Terapia anticoagulante",
  antipertensivo: "Terapia antipertensiva",
  ipolipemizzante: "Terapia ipolipemizzante",
  ipoglicemizzante_orale: "Ipoglicemizzante orale",
  insulina: "Terapia insulinica",
  m_rs_pre_evento_0_5: "mRS pre-evento",
  nihss_allingresso_numeric: "NIHSS all’ingresso",
  nihss_24h_numeric: "NIHSS a 24 ore",
  gcs_allingresso: "GCS all’ingresso",
  wake_up_stroke_si_no: "Wake-up stroke",
  modalita_di_arrivo_in_ps_118_1_autopresentazione_2_altro_3: "Modalità di arrivo in PS",
  onset_to_door_min_recalc: "Onset-to-door (min)",
  door_to_imaging_min_recalc: "Door-to-imaging (min)",
  door_to_needle_min_recalc: "Door-to-needle (min)",
  onset_to_needle_min_recalc: "Onset-to-needle (min)",
  onset_to_groin_min_recalc: "Onset-to-groin (min)",
  door_to_groin_min_recalc: "Door-to-groin (min)",
  groin_to_tici_min_recalc: "Groin-to-mTICI (min)",
  onset_to_tici_min_recalc: "Onset-to-mTICI (min)",
  ivt_0_no_1_si: "Trattamento IVT",
  evt_si_no: "Trattamento EVT",
  m_tici_grade: "Grado mTICI",
  successful_recanalization: "Ricanalizzazione efficace",
};

const binaryFeatureVariables = new Set([
  "ipertensione_arteriosa", "diabete", "pregresso_ictus_tia", "pregresso_ima",
  "dislipidemia", "trombofilia", "abuso_di_alcol", "uso_di_sostanze_stupefacenti",
  "fibrillazione_atriale", "ateromasia_carotidea_50_percent", "ateromasia_vertebrale",
  "patologia_neoplastica", "utilizzo_contraccettivi_orali", "familirita_per_ictus",
  "antiaggregante", "anticoagulante", "antipertensivo", "ipolipemizzante",
  "ipoglicemizzante_orale", "insulina", "wake_up_stroke_si_no", "ivt_0_no_1_si", "evt_si_no",
]);

const preMrsLabels = {
  0: "0 — Nessun sintomo",
  1: "1 — Nessuna disabilità significativa",
  2: "2 — Disabilità lieve",
  3: "3 — Disabilità moderata",
  4: "4 — Disabilità moderatamente grave",
  5: "5 — Disabilità grave",
};

function displayFeatureLabel(name) {
  if (clinicalFeatureLabels[name]) return clinicalFeatureLabels[name];
  const base = Object.keys(clinicalFeatureLabels)
    .sort((a, b) => b.length - a.length)
    .find((candidate) => String(name).startsWith(candidate));
  if (!base) return cleanLabel(name);
  const suffix = String(name).slice(base.length).replace(/^_+/, "");
  if (!suffix) return clinicalFeatureLabels[base];
  if (suffix.includes("missing_flagmissing")) return `${clinicalFeatureLabels[base]} · dato mancante`;
  return `${clinicalFeatureLabels[base]} · ${displayCategoricalValue(base, suffix)}`;
}

function displayInputLabel(input) {
  return clinicalFeatureLabels[input.variable] || input.label || cleanLabel(input.variable);
}

function displayCategoricalValue(variable, value) {
  const text = String(value ?? "");
  if (text === "missing_or_not_applicable") return "Non disponibile / non applicabile";
  if (variable === "sesso_m_0_f_1") return Number(value) === 1 ? "Donna" : "Uomo";
  if (variable === "m_rs_pre_evento_0_5") return preMrsLabels[value] || String(value);
  if (binaryFeatureVariables.has(variable) && ["0", "1"].includes(text)) return Number(value) === 1 ? "Sì" : "No";
  if (variable === "fumo_0_no_1_attivo_2_ex") {
    return ({ 0: "Non fumatore", 1: "Fumatore attivo", 2: "Ex fumatore" })[text] || text;
  }
  if (variable === "ischemico_0_emorragico_1_tia_2_altro_3") {
    return ({ 0: "Ischemico", 1: "Emorragico", 2: "TIA", 3: "Altro" })[text] || text;
  }
  if (variable === "modalita_di_arrivo_in_ps_118_1_autopresentazione_2_altro_3") {
    return ({ 118: "118 / soccorso territoriale", 1: "Autopresentazione", 2: "Altro mezzo", 3: "Altro" })[text] || text;
  }
  if (variable === "successful_recanalization") {
    return ({ si: "Sì", no: "No" })[text.toLowerCase()] || text;
  }
  return text;
}

function displayAllowedValue(variable, value) {
  return displayCategoricalValue(variable, value);
}

function displayFeatureValue(variable, value) {
  if (value === null || value === undefined || value === "") return "";
  if (variable === "sesso_m_0_f_1") return Number(value) === 1 ? "Donna" : "Uomo";
  if (variable === "m_rs_pre_evento_0_5") return preMrsLabels[Math.round(Number(value))] || String(value);
  if (binaryFeatureVariables.has(variable) || [
    "fumo_0_no_1_attivo_2_ex",
    "ischemico_0_emorragico_1_tia_2_altro_3",
    "modalita_di_arrivo_in_ps_118_1_autopresentazione_2_altro_3",
    "successful_recanalization",
  ].includes(variable)) return displayCategoricalValue(variable, value);
  return fmtNum(value, 2) || String(value);
}

function imputationForFeature(feature, result = state.result) {
  const items = Array.isArray(result?.imputation) ? result.imputation : [];
  return items.find((item) => item.variable === feature) || null;
}

function displayScale(scale) {
  if (scale === "log-odds") return "scala log-odds";
  if (scale === "scala outcome") return "scala dell’output";
  return String(scale || "scala del modello");
}

function logistic(value) {
  return 1 / (1 + Math.exp(-Number(value)));
}

function truncate(text, n = 48) {
  const s = String(text ?? "");
  return s.length > n ? `${s.slice(0, n - 1)}…` : s;
}

function riskTier(value, isClassification) {
  if (!Number.isFinite(value)) return { key: "neutral", label: "Output non disponibile" };
  return {
    key: "neutral",
    label: isClassification ? "Probabilità del modello — nessuna soglia clinica" : "Stima continua del modello",
  };
}

function escapeXml(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&apos;");
}

async function api(path, body = null) {
  const requestBody = body ? { ...body } : null;
  const options = requestBody
    ? { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(requestBody) }
    : {};
  let lastEmptyResponse = null;
  for (let attempt = 0; attempt < 3; attempt += 1) {
    const res = await fetch(path, options);
    const text = await res.text();
    if (!text.trim()) {
      lastEmptyResponse = res;
      if (res.ok && requestBody && attempt < 2) {
        await sleep(300 + attempt * 700);
        continue;
      }
      const err = new Error(`Risposta vuota da ${path} (HTTP ${res.status}).`);
      err.emptyResponse = true;
      err.path = path;
      err.status = res.status;
      throw err;
    }
    let data = null;
    try {
      data = JSON.parse(text);
    } catch (err) {
      const preview = text.replace(/\s+/g, " ").slice(0, 260);
      throw new Error(`Risposta non JSON da ${path} (HTTP ${res.status}): ${preview || err.message}`);
    }
    if (!res.ok || data.error) throw new Error(data.error || `HTTP ${res.status}`);
    return data;
  }
  const err = new Error(`Risposta vuota da ${path} (HTTP ${lastEmptyResponse?.status ?? "sconosciuto"}).`);
  err.emptyResponse = true;
  err.path = path;
  err.status = lastEmptyResponse?.status;
  throw err;
}

function sleep(ms) {
  return new Promise((resolve) => window.setTimeout(resolve, ms));
}

async function predictApi(body) {
  const requestId = `predict_${Date.now()}_${Math.random().toString(36).slice(2)}`;
  return api("/api/predict", { ...body, request_id: requestId });
}

let toastTimer = null;

function showToast(message) {
  window.clearTimeout(toastTimer);
  els.toast.textContent = message;
  els.toast.hidden = false;
  toastTimer = window.setTimeout(() => {
    els.toast.hidden = true;
  }, 9000);
}

function defaultsForTask(task) {
  const values = {};
  for (const input of task.inputs) {
    values[input.variable] = "";
  }
  return values;
}

function hasOwn(obj, key) {
  return Object.prototype.hasOwnProperty.call(obj || {}, key);
}

function memoryKey(
  recordId = els.recordSelect?.value || "Manuale",
  taskId = currentTask()?.id || state.task?.id || "unknown-task",
) {
  const recordKey = recordId && recordId !== "Manuale" ? `record:${recordId}` : "Manuale";
  return `${taskId}:${recordKey}`;
}

function rememberedValues(recordId = els.recordSelect?.value || "Manuale", taskId = currentTask()?.id) {
  const key = memoryKey(recordId, taskId);
  if (!state.valueMemory[key]) state.valueMemory[key] = {};
  return state.valueMemory[key];
}

function rememberValue(variable, value, recordId = els.recordSelect?.value || "Manuale", taskId = currentTask()?.id) {
  rememberedValues(recordId, taskId)[variable] = value;
}

function valuesForTask(task, baseValues = null, recordId = els.recordSelect?.value || "Manuale") {
  const next = defaultsForTask(task);
  const applyValues = (values) => {
    for (const input of task.inputs || []) {
      if (hasOwn(values, input.variable)) next[input.variable] = values[input.variable];
    }
  };
  applyValues(baseValues);
  applyValues(rememberedValues(recordId, task.id));
  return next;
}

function hiddenVars(task, values) {
  const raw = new Set(task.inputs.map((x) => x.variable));
  const hidden = [];
  if (raw.has("ivt_0_no_1_si") && !isYes(values.ivt_0_no_1_si)) {
    hidden.push(...ivtTimingVars.filter((x) => raw.has(x)));
  }
  if (raw.has("evt_si_no") && !isYes(values.evt_si_no)) {
    hidden.push(...evtTimingVars.filter((x) => raw.has(x)));
    if (raw.has("m_tici_grade")) hidden.push("m_tici_grade");
  }
  return Array.from(new Set(hidden));
}

function modelOptionsForTask(task) {
  if (!task) return [];
  if (Array.isArray(task.model_options) && task.model_options.length) return task.model_options;
  return [
    {
      model_id: task.model_id || "default",
      model_label: task.model_label,
      option_label: task.model_label,
      is_best: true,
      performance: task.performance,
      outcome_type: task.outcome_type,
      positive_label: task.positive_label,
      units: task.units,
      selected_feature_count: task.selected_feature_count,
      metrics: task.metrics || [],
      inputs: task.inputs,
      record_ids: task.record_ids,
      model_badges: task.model_badges || [],
      landmark: task.landmark,
      selection_criterion: task.selection_criterion,
      limitations: task.limitations,
      cohort_summary: task.cohort_summary,
      explanation: task.explanation,
    },
  ];
}

function validationEvidenceNote(task) {
  if (task?.id === "mrs3m_class_24h") {
    return "IC bootstrap 95% sulle predizioni OOF mediate, condizionati al workflow; non includono l’incertezza della selezione del modello.";
  }
  return "Stime di validazione interna della procedura; non rappresentano l’incertezza della singola predizione.";
}

function currentTask() {
  if (!state.task) return null;
  const model = state.model || modelOptionsForTask(state.task)[0] || {};
  return {
    ...state.task,
    ...model,
    id: state.task.id,
    title: state.task.title,
    model_id: model.model_id || state.task.model_id,
    model_label: model.model_label || state.task.model_label,
    performance: model.performance || state.task.performance,
    outcome_type: model.outcome_type || state.task.outcome_type,
    positive_label: model.positive_label ?? state.task.positive_label,
    units: model.units ?? state.task.units,
    selected_feature_count: model.selected_feature_count ?? state.task.selected_feature_count,
    metrics: model.metrics || state.task.metrics || [],
    inputs: model.inputs || state.task.inputs || [],
    record_ids: model.record_ids || state.task.record_ids || [],
    model_badges: model.model_badges || state.task.model_badges || [],
    landmark: model.landmark ?? state.task.landmark,
    selection_criterion: model.selection_criterion ?? state.task.selection_criterion,
    limitations: model.limitations ?? state.task.limitations,
    cohort_summary: model.cohort_summary ?? state.task.cohort_summary,
    is_best_model: Boolean(model.is_best),
  };
}

function renderTaskSelect() {
  els.taskSelect.innerHTML = "";
  for (const task of state.metadata.tasks) {
    const opt = document.createElement("option");
    opt.value = task.id;
    opt.textContent = task.title;
    els.taskSelect.appendChild(opt);
  }
  const preferred = state.metadata.tasks.find((x) => x.id === "mrs3m_class_24h") || state.metadata.tasks[0];
  els.taskSelect.value = preferred.id;
}

function renderModelSelect() {
  if (!els.modelSelect) return;
  const options = modelOptionsForTask(state.task);
  els.modelSelect.innerHTML = "";
  for (const model of options) {
    const opt = document.createElement("option");
    opt.value = model.model_id;
    opt.textContent = model.option_label || `${model.model_label}${model.is_best ? " (migliore)" : ""}`;
    els.modelSelect.appendChild(opt);
  }
  const preferred = options.find((x) => x.is_best) || options[0];
  if (preferred) {
    state.model = preferred;
    els.modelSelect.value = preferred.model_id;
  }
}

function renderRecordSelect() {
  els.recordSelect.innerHTML = "";
  const task = currentTask();
  const manual = document.createElement("option");
  manual.value = "Manuale";
  manual.textContent = "Manuale";
  els.recordSelect.appendChild(manual);
  if ((task.record_ids || []).length) throw new Error("Artefatto non sicuro: contiene identificativi di record paziente.");
}

function renderModelNotes() {
  const t = currentTask();
  const metrics = Array.isArray(t.metrics) ? t.metrics : [];
  const badges = Array.isArray(t.model_badges)
    ? t.model_badges
    : (t.model_badges ? [t.model_badges] : []);
  const isExplicitDefault = badges.some((badge) => String(badge).toLowerCase().startsWith("default"));
  const bestLabel = t.is_best_model ? (isExplicitDefault ? " (default)" : " (migliore)") : "";
  const cohort = t.cohort_summary || null;
  const cohortText = cohort && cohort.n
    ? `${cohort.n} pazienti${cohort.events !== undefined ? `; ${cohort.events} eventi` : ""}`
    : "";
  const primaryMetricIds = new Set(["auc", "pr_auc", "brier", "rmse", "mae", "r2"]);
  const primaryMetrics = metrics.filter((metric) => primaryMetricIds.has(metric.metric));
  const secondaryMetrics = metrics.filter((metric) => !primaryMetricIds.has(metric.metric));
  const metricChips = (items, primary = false) => items
    .map((metric) => {
      const hasInterval = /\[[^\]]+\]/.test(String(metric.estimate_ci || ""));
      return `
        <div class="metric-chip${primary ? " primary" : ""}">
          <span>${escapeXml(metric.metric_label || metric.metric || "")}</span>
          <strong>${escapeXml(metric.estimate_ci || "")}</strong>
          <em>${hasInterval ? "IC 95%" : "stima puntuale"}</em>
        </div>
      `;
    })
    .join("");
  const metricsHtml = metrics.length
    ? `
      <div class="metrics-block">
        <div class="metrics-title-row">
          <div>
            <span class="metrics-title">Performance interna</span>
            <small>${escapeXml(validationEvidenceNote(t))}</small>
          </div>
          <span class="evidence-pill">Validazione interna</span>
        </div>
        <div class="metric-grid primary-metrics">${metricChips(primaryMetrics.length ? primaryMetrics : metrics, true)}</div>
        ${secondaryMetrics.length ? `
          <details class="metric-details">
            <summary>Calibrazione e metriche esplorative</summary>
            <div class="metric-grid secondary-metrics">${metricChips(secondaryMetrics)}</div>
          </details>
        ` : ""}
      </div>
    `
    : "";
  const noteRow = (key, value, wide = false) => value
    ? `<div class="note-row${wide ? " note-wide" : ""}"><span class="note-key">${escapeXml(key)}</span><span class="note-value">${escapeXml(value)}</span></div>`
    : "";
  const limitationItems = t.limitations
    ? String(t.limitations)
        .replaceAll(". ", ".;")
        .split(";")
        .map((item) => item.trim())
        .filter(Boolean)
    : [];
  const limitationsHtml = limitationItems.length
    ? `
      <div class="limitations-card">
        <div class="limitations-title">Limiti da considerare</div>
        <ul>${limitationItems.map((item) => `<li>${escapeXml(item)}</li>`).join("")}</ul>
      </div>
    `
    : "";
  els.modelNotes.innerHTML = `
    <div class="model-note-grid">
      ${badges.length ? noteRow("Ruolo", badges.join("; ")) : ""}
      ${noteRow("Modello", `${t.model_label}${bestLabel}`)}
      ${noteRow("Landmark", t.landmark)}
      ${noteRow("Coorte", cohortText)}
      ${noteRow("Predittori", `${t.selected_feature_count}`)}
      ${noteRow("Spiegazione locale", t.explanation?.method)}
      ${noteRow("Deployment", "Refit sperimentale non validato indipendentemente")}
      ${noteRow("Scenario", t.scenario, true)}
      ${noteRow("Criterio di selezione", t.selection_criterion, true)}
      ${noteRow("Procedura CV", t.performance, true)}
    </div>
    ${limitationsHtml}
    ${metricsHtml}
  `;
  const approximate = Boolean(t.explanation?.approximate);
  els.explanationBadge.textContent = approximate ? "Stima approssimata" : "Decomposizione esatta";
  els.explanationNote.textContent = approximate
    ? `${t.explanation?.note || "Stima permutation-SHAP Monte Carlo."} I contributi descrivono associazioni del modello, non effetti protettivi o causali.`
    : "Decomposizione additiva esatta sulla scala del modello. I contributi descrivono associazioni, non effetti protettivi o causali.";
}

function renderInputs() {
  const task = currentTask();
  const hidden = new Set(hiddenVars(task, state.values));
  const hiddenList = Array.from(hidden);
  const conditionalVariables = new Set([...ivtTimingVars, ...evtTimingVars, "m_tici_grade"]);
  const hasConditionalInputs = task.inputs.some((input) => conditionalVariables.has(input.variable));
  els.conditionalBadge.textContent = hiddenList.length
    ? `${hiddenList.length} campi non applicabili`
    : (hasConditionalInputs ? "Timing coerenti con il percorso" : `${task.inputs.length} variabili`);
  renderPatientContext();
  els.inputGrid.innerHTML = "";

  for (const input of task.inputs) {
    if (hidden.has(input.variable)) continue;
    const cell = document.createElement("div");
    cell.className = "input-cell";
    cell.dataset.variable = input.variable;
    const labelRow = document.createElement("div");
    labelRow.className = "input-label-row";
    const label = document.createElement("label");
    label.textContent = displayInputLabel(input);
    label.htmlFor = `in_${input.variable}`;
    labelRow.appendChild(label);
    if (input.allow_missing) {
      const optional = document.createElement("span");
      optional.className = "optional-badge";
      optional.textContent = input.type === "numeric"
        ? `missing → mediana ${fmtCompact(input.imputation_value)}`
        : "missing esplicito";
      labelRow.appendChild(optional);
    }
    cell.appendChild(labelRow);

    let control;
    if (input.type === "numeric" && Array.isArray(input.allowed_values) && input.allowed_values.length) {
      control = document.createElement("select");
      const blank = document.createElement("option");
      blank.value = "";
      blank.textContent = input.allow_missing ? "-- Seleziona / lascia missing --" : "-- Seleziona --";
      control.appendChild(blank);
      for (const allowed of input.allowed_values) {
        const opt = document.createElement("option");
        opt.value = String(allowed);
        opt.textContent = displayAllowedValue(input.variable, allowed);
        control.appendChild(opt);
      }
      control.value = state.values[input.variable] ?? "";
    } else if (input.type === "numeric") {
      control = document.createElement("input");
      control.type = "number";
      control.step = "any";
      control.value = state.values[input.variable] ?? "";
      if (Number.isFinite(Number(input.min))) control.min = String(input.min);
      if (Number.isFinite(Number(input.max))) control.max = String(input.max);
      control.placeholder = input.allow_missing
        ? `Vuoto = imputazione mediana ${fmtCompact(input.imputation_value)}`
        : "Valore richiesto";
    } else {
      control = document.createElement("select");
      const blank = document.createElement("option");
      blank.value = "";
      blank.textContent = input.allow_missing ? "-- Seleziona, anche se non disponibile --" : "-- Seleziona --";
      control.appendChild(blank);
      for (const level of input.levels || []) {
        const opt = document.createElement("option");
        opt.value = level;
        opt.textContent = displayCategoricalValue(input.variable, level);
        control.appendChild(opt);
      }
      control.value = state.values[input.variable] ?? "";
    }
    control.id = `in_${input.variable}`;
    control.dataset.variable = input.variable;
    const requiresExplicitSelection = input.type !== "numeric";
    control.required = !input.allow_missing || requiresExplicitSelection;
    control.setAttribute("aria-required", String(control.required));
    const updateEvent = control.tagName === "SELECT" ? "change" : "input";
    control.addEventListener(updateEvent, onInputChange);
    control.addEventListener("invalid", () => {
      cell.classList.add("is-invalid");
      control.setAttribute("aria-invalid", "true");
    });
    cell.appendChild(control);
    if (input.warn_outside_training_range &&
        Number.isFinite(Number(input.training_min)) &&
        Number.isFinite(Number(input.training_max))) {
      const hint = document.createElement("small");
      hint.className = "training-range-hint";
      hint.textContent = `Intervallo osservato nel training: ${input.training_min}–${input.training_max}`;
      cell.appendChild(hint);
    }
    els.inputGrid.appendChild(cell);
  }

  if (hiddenList.length) {
    const notice = document.createElement("div");
    notice.className = "input-cell notice-cell";
    notice.innerHTML = `<div class="eyebrow">Non applicabili</div><p>${hiddenList.map(displayFeatureLabel).map(escapeXml).join(", ")}</p>`;
    els.inputGrid.appendChild(notice);
  }
}

function renderPatientContext() {
  if (!els.recordSummaryValue || !els.pathwaySummaryValue) return;
  const record = els.recordSelect?.value || "Manuale";
  const inputNames = new Set((currentTask()?.inputs || []).map((input) => input.variable));
  const hasPathway = inputNames.has("ivt_0_no_1_si") || inputNames.has("evt_si_no");
  const ivt = isYes(state.values.ivt_0_no_1_si);
  const evt = isYes(state.values.evt_si_no);
  const pathway = hasPathway
    ? ([ivt ? "IVT" : null, evt ? "EVT" : null].filter(Boolean).join(" + ") || "Nessun trattamento indicato negli input")
    : "Non incluso nel task";
  els.recordSummaryValue.textContent = record;
  els.pathwaySummaryValue.textContent = pathway;
}

function onInputChange(event) {
  const variable = event.target.dataset.variable;
  event.target.removeAttribute("aria-invalid");
  event.target.closest(".input-cell")?.classList.remove("is-invalid");
  state.values[variable] = event.target.value;
  rememberValue(variable, event.target.value, els.recordSelect?.value || "Manuale", currentTask()?.id);
  if (["ivt_0_no_1_si", "evt_si_no"].includes(variable)) {
    renderInputs();
    window.requestAnimationFrame(() => document.getElementById(`in_${variable}`)?.focus({ preventScroll: true }));
  }
}

function setLoading(on) {
  els.predictButton.disabled = on;
  els.taskSelect.disabled = on;
  els.modelSelect.disabled = on;
  els.predictionForm?.setAttribute("aria-busy", String(on));
  els.summaryPanel?.setAttribute("aria-busy", String(on));
  els.forcePlot?.setAttribute("aria-busy", String(on));
  els.violinPlot?.setAttribute("aria-busy", String(on));
  els.predictButton.innerHTML = on
    ? "<span class='button-spinner' aria-hidden='true'></span> Calcolo..."
    : "<svg viewBox='0 0 24 24' aria-hidden='true'><path d='M8 5.5v13l10-6.5-10-6.5Z'/></svg> Calcola predizione";
  if (on && !state.result) els.forcePlot.textContent = "Calcolo della predizione...";
}

function renderSummary(result) {
  const task = result.task;
  const value = Number(result.prediction.value);
  const isClassification = task.outcome_type === "classification";
  const main = isClassification ? fmtPct(value) : fmtNum(value, 2);
  const label = isClassification ? `Probabilità stimata: ${task.positive_label}` : `Stima ${task.units || ""}`;
  const width = isClassification ? Math.max(0, Math.min(100, value * 100)) : 50;
  const ivt = isYes(result.values.ivt_0_no_1_si);
  const evt = isYes(result.values.evt_si_no);
  const hasPathway = Object.prototype.hasOwnProperty.call(result.values, "ivt_0_no_1_si") ||
    Object.prototype.hasOwnProperty.call(result.values, "evt_si_no");
  const tier = riskTier(value, isClassification);
  const metrics = Array.isArray(task.metrics) ? task.metrics : [];
  const preferredMetrics = ["auc", "pr_auc", "brier", "rmse", "mae", "r2"];
  const evidenceMetrics = preferredMetrics
    .map((id) => metrics.find((metric) => metric.metric === id))
    .filter(Boolean)
    .slice(0, 3);
  const imputation = Array.isArray(result.imputation) ? result.imputation : [];
  const imputationText = imputation
    .map((item) => `${displayFeatureLabel(item.variable)} → ${displayFeatureValue(item.variable, item.value)}`)
    .join("; ");
  renderPatientContext();

  els.summaryPanel.innerHTML = `
    <div class="prediction-output-layout">
      <article class="prediction-copy">
        <div class="metric-label">${label}</div>
        <div class="metric-main">${main}</div>
        <span class="risk-badge ${tier.key}">${escapeXml(tier.label)}</span>
        ${isClassification ? `
          <div class="probability-scale" aria-label="Probabilità del modello ${main}">
            <span class="probability-fill" style="width:${width.toFixed(1)}%"></span>
          </div>
          <div class="probability-labels" aria-hidden="true"><span>0%</span><span>100%</span></div>
        ` : ""}
        <p class="output-disclaimer">Output sperimentale per ricerca. Non è una categoria di rischio né un’indicazione terapeutica.</p>
        ${imputationText ? `<div class="imputation-note"><strong>Missing imputati</strong><span>${escapeXml(imputationText)}</span></div>` : ""}
        ${hasPathway ? `<div class="pathway-strip">
          <span class="path-pill ${ivt ? "on" : "off"}">IVT: ${ivt ? "sì" : "no"}</span>
          <span class="path-pill ${evt ? "on" : "off"}">EVT: ${evt ? "sì" : "no"}</span>
        </div>` : ""}
      </article>
      <article class="evidence-card">
        <div class="evidence-card-heading">
          <span>Performance interna della procedura</span>
          <strong>${escapeXml(task.model_label || "Modello selezionato")}</strong>
        </div>
        ${evidenceMetrics.length ? `<div class="evidence-grid">
          ${evidenceMetrics.map((metric) => `
            <div><span>${escapeXml(metric.metric_label || metric.metric)}</span><strong>${escapeXml(metric.estimate_ci || "")}</strong></div>
          `).join("")}
        </div>` : "<p>Metriche sintetiche non disponibili.</p>"}
        <p>${escapeXml(task.performance || "Nested cross-validation")}</p>
        <small>${escapeXml(validationEvidenceNote(task))}</small>
      </article>
    </div>
  `;
  pulse(els.summaryPanel);
}

function pulse(el) {
  el.classList.remove("pulse");
  void el.offsetWidth;
  el.classList.add("pulse");
}

function buildForceSegments(shap, maxFeatures = 9) {
  const clean = shap
    .filter((d) => Number.isFinite(Number(d.contribution)))
    .map((d) => ({ ...d, contribution: Number(d.contribution), value: d.value }))
    .sort((a, b) => Math.abs(b.contribution) - Math.abs(a.contribution));
  if (!clean.length) return null;
  const base = Number(clean[0].base_value);
  const pred = Number(clean[0].prediction_link);
  const scale = clean[0].shap_scale || "scala outcome";
  let rows = clean;
  if (rows.length > maxFeatures) {
    const top = rows.slice(0, maxFeatures);
    const other = rows.slice(maxFeatures).reduce((s, d) => s + d.contribution, 0);
    rows = [...top, { feature: "Altre feature", value: null, contribution: other, base_value: base, prediction_link: pred, shap_scale: scale }];
  }
  rows = rows.filter((d) => Math.abs(d.contribution) > 1e-10);
  const neg = rows.filter((d) => d.contribution < 0).sort((a, b) => a.contribution - b.contribution);
  const pos = rows.filter((d) => d.contribution >= 0).sort((a, b) => b.contribution - a.contribution);
  const segments = [];
  let current = base;
  for (const row of neg) {
    const next = current + row.contribution;
    segments.push({ ...row, x0: next, x1: current, direction: "Riduce predizione" });
    current = next;
  }
  current = base + neg.reduce((s, d) => s + d.contribution, 0);
  for (const row of pos) {
    const next = current + row.contribution;
    segments.push({ ...row, x0: current, x1: next, direction: "Aumenta predizione" });
    current = next;
  }
  return { segments, base, pred, scale };
}

function renderForcePlot(shap) {
  const data = buildForceSegments(shap);
  if (!data || !data.segments.length) {
    const base = shap?.length ? Number(shap[0].base_value) : null;
    const pred = shap?.length ? Number(shap[0].prediction_link) : null;
    const same = Number.isFinite(base) && Number.isFinite(pred) && Math.abs(base - pred) < 1e-8;
    els.forcePlot.innerHTML = `
      <div class="explain-empty">
        <div class="empty-title">Nessun contributo SHAP diverso da zero</div>
        <p>
          Per questo task il modello selezionato non sta usando le feature per spostare la predizione:
          la stima coincide con il valore base${same ? "" : " o tutti i contributi sono numericamente nulli"}.
        </p>
        <div class="empty-metrics">
          <span>Base: ${Number.isFinite(base) ? fmtNum(base, 3) : "NA"}</span>
          <span>Predizione: ${Number.isFinite(pred) ? fmtNum(pred, 3) : "NA"}</span>
        </div>
        <p class="empty-note">
          Nei modelli penalizzati questo accade quando la cross-validation sceglie una penalizzazione che azzera tutti
          i coefficienti non-intercetta. La predizione resta disponibile, ma non ha una spiegazione paziente-specifica.
        </p>
      </div>
    `;
    return;
  }

  const w = 1060;
  const h = 480;
  const margin = { left: 70, right: 70, top: 72, bottom: 72 };
  const xs = [data.base, data.pred, ...data.segments.flatMap((s) => [s.x0, s.x1])];
  let min = Math.min(...xs);
  let max = Math.max(...xs);
  let range = max - min;
  if (!Number.isFinite(range) || range === 0) range = 1;
  min -= range * 0.14;
  max += range * 0.14;
  const x = (v) => margin.left + ((v - min) / (max - min)) * (w - margin.left - margin.right);
  const axisY = 330;
  const barTop = 190;
  const barMid = 220;
  const barBottom = 250;
  const colors = {
    "Aumenta predizione": "#d6a72d",
    "Riduce predizione": "#a12c5e",
  };

  function arrowPolygon(seg) {
    const left = Math.min(x(seg.x0), x(seg.x1));
    const right = Math.max(x(seg.x0), x(seg.x1));
    const width = Math.max(2, right - left);
    const head = Math.min(width * 0.42, 24);
    if (seg.contribution >= 0) {
      return `${left},${barTop} ${right - head},${barTop} ${right},${barMid} ${right - head},${barBottom} ${left},${barBottom}`;
    }
    return `${left},${barMid} ${left + head},${barTop} ${right},${barTop} ${right},${barBottom} ${left + head},${barBottom}`;
  }

  const grid = [];
  for (let i = 0; i <= 5; i++) {
    const gx = margin.left + (i / 5) * (w - margin.left - margin.right);
    const val = min + (i / 5) * (max - min);
    grid.push(`<line x1="${gx}" y1="120" x2="${gx}" y2="${axisY}" stroke="#e8edf4"/>`);
    grid.push(`<text x="${gx}" y="${axisY + 28}" text-anchor="middle" fill="#667085" font-size="13">${fmtNum(val, 2)}</text>`);
  }

  const labelLanes = [94, 120, 146, 172];
  const laneOccupancy = labelLanes.map(() => []);
  const labelLayout = new Map();
  const labelCandidates = data.segments
    .map((seg, index) => {
      const mid = (x(seg.x0) + x(seg.x1)) / 2;
      const imputed = imputationForFeature(seg.feature);
      const featureValue = imputed
        ? `=missing → imputato a ${displayFeatureValue(imputed.variable, imputed.value)}`
        : (seg.value === null || seg.value === undefined || seg.value === ""
            ? ""
            : `=${displayFeatureValue(seg.feature, seg.value)}`);
      const label = truncate(`${displayFeatureLabel(seg.feature)}${featureValue}`, 34);
      const width = Math.min(230, Math.max(58, label.length * 7.1 + 16));
      return { index, seg, mid, label, width, importance: Math.abs(seg.contribution) };
    })
    .filter((d) => d.importance > 1e-6)
    .sort((a, b) => b.importance - a.importance);

  for (const item of labelCandidates) {
    let placed = false;
    const targetX = Math.max(margin.left + item.width / 2, Math.min(w - margin.right - item.width / 2, item.mid));
    for (let lane = 0; lane < labelLanes.length; lane++) {
      const collisions = laneOccupancy[lane].some((other) => {
        const minDistance = (item.width + other.width) / 2 + 18;
        return Math.abs(targetX - other.x) < minDistance;
      });
      if (!collisions) {
        laneOccupancy[lane].push({ x: targetX, width: item.width });
        labelLayout.set(item.index, {
          x: targetX,
          y: labelLanes[lane],
          label: item.label,
          width: item.width,
        });
        placed = true;
        break;
      }
    }
    if (!placed) continue;
  }

  const segs = data.segments
    .map((seg, i) => {
      const mid = (x(seg.x0) + x(seg.x1)) / 2;
      const width = Math.abs(x(seg.x1) - x(seg.x0));
      const contribLabel = width > 45 ? `${seg.contribution >= 0 ? "+" : ""}${fmtNum(seg.contribution, 3)}` : "";
      const textColor = seg.direction === "Aumenta predizione" ? "#111827" : "#ffffff";
      const label = labelLayout.get(i);
      const labelSvg = label
        ? `
          <polyline points="${mid},${barTop - 8} ${label.x},${label.y + 8}" fill="none" stroke="#c7d3e1" stroke-width="1"/>
          <rect x="${label.x - label.width / 2}" y="${label.y - 17}" width="${label.width}" height="22" rx="5" fill="rgba(255,255,255,0.88)" stroke="#dbe5ef"/>
          <text x="${label.x}" y="${label.y}" text-anchor="middle" fill="#253247" font-size="12">${escapeXml(label.label)}</text>
        `
        : "";
      return `
        ${labelSvg}
        <polygon points="${arrowPolygon(seg)}" fill="${colors[seg.direction]}" stroke="#111827" stroke-width="1"/>
        <text x="${mid}" y="${barMid + 6}" text-anchor="middle" fill="${textColor}" font-size="18" font-weight="760">${contribLabel}</text>
      `;
    })
    .join("");

  const isLogOdds = String(data.scale).toLowerCase().includes("log");
  const baseDisplay = isLogOdds ? `${fmtNum(data.base, 3)} · ${fmtPct(logistic(data.base))}` : fmtNum(data.base, 3);
  const predictionDisplay = isLogOdds ? `${fmtNum(data.pred, 3)} · ${fmtPct(logistic(data.pred))}` : fmtNum(data.pred, 3);
  const chartDescription = `Contributi locali del modello. Valore base ${baseDisplay}; output ${predictionDisplay}.`;
  els.forcePlot.innerHTML = `
    <svg viewBox="0 0 ${w} ${h}" role="img" aria-labelledby="forceTitle forceDescription">
      <title id="forceTitle">Contributi locali SHAP</title>
      <desc id="forceDescription">${escapeXml(chartDescription)}</desc>
      <text x="${margin.left}" y="34" fill="#111827" font-size="26" font-weight="760">Contributi locali SHAP</text>
      <text x="${margin.left}" y="58" fill="#667085" font-size="14">${escapeXml(state.result.task.model_label)} · ${escapeXml(displayScale(data.scale))}</text>
      ${grid.join("")}
      <line x1="${margin.left}" y1="${axisY}" x2="${w - margin.right}" y2="${axisY}" stroke="#111827" stroke-width="2"/>
      <line x1="${x(data.base)}" y1="110" x2="${x(data.base)}" y2="${axisY}" stroke="#111827" stroke-dasharray="5 5" stroke-width="1.5"/>
      <line x1="${x(data.pred)}" y1="110" x2="${x(data.pred)}" y2="${axisY}" stroke="#111827" stroke-width="1.8"/>
      ${segs}
      <text x="${x(data.base)}" y="${axisY - 28}" text-anchor="${data.base > data.pred ? "start" : "end"}" fill="#1f2937" font-size="14">Valore base: ${escapeXml(baseDisplay)}</text>
      <text x="${x(data.pred)}" y="${axisY - 8}" text-anchor="${data.pred > data.base ? "start" : "end"}" fill="#1f2937" font-size="14">Output: ${escapeXml(predictionDisplay)}</text>
      <text x="${w / 2}" y="${h - 22}" text-anchor="middle" fill="#1f2937" font-size="16" font-weight="720">Contributo locale (${escapeXml(displayScale(data.scale))})</text>
      <rect x="${w / 2 - 128}" y="${h - 55}" width="14" height="14" fill="${colors["Aumenta predizione"]}"/>
      <text x="${w / 2 - 108}" y="${h - 43}" fill="#344054" font-size="13">Aumenta predizione</text>
      <rect x="${w / 2 + 52}" y="${h - 55}" width="14" height="14" fill="${colors["Riduce predizione"]}"/>
      <text x="${w / 2 + 72}" y="${h - 43}" fill="#344054" font-size="13">Riduce predizione</text>
    </svg>
  `;
  pulse(els.forcePlot);
}

function renderViolinPlot(payload) {
  const summary = (payload?.summary || [])
    .map((row) => ({ ...row, mean_abs_shap: Number(row.mean_abs_shap) }))
    .filter((row) => Number.isFinite(row.mean_abs_shap))
    .sort((a, b) => b.mean_abs_shap - a.mean_abs_shap)
    .slice(0, 10);
  if (!summary.length) {
    els.violinPlot.innerHTML = "<div class='chart-empty'>Profilo SHAP non disponibile per questo task.</div>";
    els.violinBadge.textContent = "Non disponibile";
    return;
  }
  const maxValue = Math.max(...summary.map((row) => row.mean_abs_shap), 1e-9);
  const profileCount = Number(payload.sampled_profiles) || 0;
  const rowH = 54;
  const w = 1060;
  const h = Math.max(350, 104 + summary.length * rowH + 64);
  const margin = { left: 260, right: 110, top: 82, bottom: 58 };
  const plotW = w - margin.left - margin.right;
  const x = (value) => margin.left + (Math.max(0, value) / maxValue) * plotW;
  const axis = [];
  for (let i = 0; i <= 4; i += 1) {
    const gx = margin.left + (i / 4) * plotW;
    const value = (i / 4) * maxValue;
    axis.push(`<line x1="${gx}" y1="${margin.top - 10}" x2="${gx}" y2="${h - margin.bottom}" stroke="#e7edf5"/>`);
    axis.push(`<text x="${gx}" y="${h - 24}" text-anchor="middle" fill="#667085" font-size="12">${fmtNum(value, 3)}</text>`);
  }
  const bars = summary.map((row, index) => {
    const center = margin.top + index * rowH + rowH / 2;
    const end = x(row.mean_abs_shap);
    const width = Math.max(3, end - margin.left);
    const opacity = Math.max(0.58, 1 - index * 0.045);
    const featureLabel = displayFeatureLabel(row.feature);
    const aria = `${featureLabel}: importanza media assoluta SHAP ${fmtNum(row.mean_abs_shap, 4)}`;
    return `
      <g aria-label="${escapeXml(aria)}">
        <text x="${margin.left - 18}" y="${center + 5}" text-anchor="end" fill="#253247" font-size="14" font-weight="650">${escapeXml(truncate(featureLabel, 34))}</text>
        <rect x="${margin.left}" y="${center - 10}" width="${plotW}" height="20" rx="10" fill="#f1f5fa"/>
        <rect x="${margin.left}" y="${center - 10}" width="${width}" height="20" rx="10" fill="#3867d6" fill-opacity="${opacity}"/>
        <circle cx="${end}" cy="${center}" r="6" fill="#0e9384" stroke="#ffffff" stroke-width="2"/>
        <text x="${w - margin.right + 14}" y="${center + 5}" fill="#344054" font-size="13" font-weight="720">${fmtNum(row.mean_abs_shap, 4)}</text>
      </g>
    `;
  }).join("");
  const approximate = Boolean(payload.explanation?.approximate);
  const methodNote = approximate ? " Stima permutation-SHAP Monte Carlo approssimata." : " Decomposizione additiva esatta.";
  const description = `Importanza media assoluta dei predittori su ${profileCount || "alcuni"} profili aggregati sintetici. Il grafico non rappresenta la distribuzione della coorte.${methodNote}`;
  const accessibleRanking = summary.map((row) => `
    <li><span>${escapeXml(displayFeatureLabel(row.feature))}</span><strong>${fmtNum(row.mean_abs_shap, 4)}</strong></li>
  `).join("");
  els.violinBadge.textContent = profileCount ? `${profileCount} profili sintetici` : "Profili sintetici";
  els.violinPlot.innerHTML = `
    <svg viewBox="0 0 ${w} ${h}" role="img" aria-labelledby="globalShapTitle globalShapDescription">
      <title id="globalShapTitle">Importanza media assoluta SHAP</title>
      <desc id="globalShapDescription">${escapeXml(description)}</desc>
      <text x="${margin.left}" y="31" fill="#111827" font-size="25" font-weight="760">Importanza media assoluta SHAP</text>
      <text x="${margin.left}" y="55" fill="#667085" font-size="13">${escapeXml(payload.model_label || "Modello selezionato")} · ${escapeXml(displayScale(payload.shap_scale))}${approximate ? " · stima approssimata" : ""}</text>
      ${axis.join("")}
      <g>${bars}</g>
      <text x="${margin.left + plotW / 2}" y="${h - 6}" text-anchor="middle" fill="#1f2937" font-size="14" font-weight="720">Media |SHAP| · ${escapeXml(displayScale(payload.shap_scale))}</text>
    </svg>
    <div class="global-shap-accessible">
      <h3>Ranking testuale dell’importanza globale</h3>
      <p>${escapeXml(description)}</p>
      <ol>${accessibleRanking}</ol>
    </div>
  `;
  pulse(els.violinPlot);
}

async function loadShapSummary() {
  const task = currentTask();
  const taskId = task.id;
  const modelId = task.model_id;
  const cacheKey = `${taskId}:${modelId}`;
  const requestVersion = ++state.shapSummaryRequestVersion;
  if (state.shapSummaryCache.has(cacheKey)) {
    if (`${currentTask()?.id}:${currentTask()?.model_id}` === cacheKey) {
      renderViolinPlot(state.shapSummaryCache.get(cacheKey));
    }
    return;
  }
  els.violinBadge.textContent = "Calcolo...";
  els.violinPlot.innerHTML = "<div class='chart-empty'>Calcolo del profilo SHAP...</div>";
  try {
    const data = await api("/api/shap-summary", {
      task_id: taskId,
      model_id: modelId,
      max_features: 10,
    });
    state.shapSummaryCache.set(cacheKey, data);
    if (requestVersion !== state.shapSummaryRequestVersion) return;
    if (`${currentTask()?.id}:${currentTask()?.model_id}` !== cacheKey) return;
    renderViolinPlot(data);
  } catch (err) {
    if (requestVersion !== state.shapSummaryRequestVersion) return;
    if (`${currentTask()?.id}:${currentTask()?.model_id}` !== cacheKey) return;
    els.violinPlot.innerHTML = `<div class='chart-empty'>${escapeXml(err.message)}</div>`;
    els.violinBadge.textContent = "Non disponibile";
  }
}

function renderTable(container, columns, rows, label) {
  if (!rows || !rows.length) {
    container.innerHTML = "<div class='table-empty'>Nessun dato disponibile.</div>";
    return;
  }
  const head = columns.map((c) => `<th scope="col">${escapeXml(c.label)}</th>`).join("");
  const body = rows
    .map((row) => {
      const cells = columns
        .map((c) => {
          const raw = c.value ? c.value(row) : row[c.key];
          const cls = c.numeric ? "number" : "";
          const value = c.html ? (raw ?? "") : escapeXml(raw ?? "");
          return `<td class="${cls}">${value}</td>`;
        })
        .join("");
      return `<tr>${cells}</tr>`;
    })
    .join("");
  container.innerHTML = `
    <div class="table-wrap" tabindex="0" aria-label="${escapeXml(label)}">
      <table>
        <caption class="sr-only">${escapeXml(label)}</caption>
        <thead><tr>${head}</tr></thead>
        <tbody>${body}</tbody>
      </table>
    </div>
  `;
}

function renderTables(result) {
  const shapRows = [...result.shap]
    .sort((a, b) => Math.abs(Number(b.contribution)) - Math.abs(Number(a.contribution)))
    .map((d) => {
      const imputed = imputationForFeature(d.feature, result);
      return {
      feature: truncate(displayFeatureLabel(d.feature), 54),
      value: imputed
        ? `missing → imputato a ${displayFeatureValue(imputed.variable, imputed.value)}`
        : displayFeatureValue(d.feature, d.value),
      contribution: Number(d.contribution),
      contributionText: fmtNum(d.contribution, 4),
      directionClass: Number(d.contribution) >= 0 ? "positive" : "negative",
      };
    });
  renderTable(
    els.shapTable,
    [
      { key: "feature", label: "Feature" },
      { key: "value", label: "Valore", numeric: true },
      {
        key: "contributionText",
        label: "Contributo",
        numeric: true,
        html: true,
        value: (r) => `<span class="${r.directionClass}">${r.contributionText}</span>`,
      },
    ],
    shapRows,
    "Contributi SHAP ordinati per importanza assoluta",
  );

  const featureRows = (result.feature_table || []).map((row) => ({
    ...row,
    selected_design_feature: displayFeatureLabel(row.selected_design_feature),
  }));
  renderTable(
    els.featureTable,
    [{ key: "selected_design_feature", label: "Predittore selezionato" }],
    featureRows,
    "Predittori utilizzati dal modello",
  );
}

function selectAvailableRecord(recordId) {
  const candidate = recordId || "Manuale";
  const exists = Array.from(els.recordSelect.options).some((opt) => opt.value === candidate);
  if (!exists) throw new Error(`Record non disponibile: ${candidate}`);
  els.recordSelect.value = candidate;
  return els.recordSelect.value;
}

async function loadValuesForRecord(task, recordId) {
  if (recordId === "Manuale") return valuesForTask(task, null, recordId);
  const data = await api("/api/record", { task_id: task.id, model_id: task.model_id, record_id: recordId });
  return valuesForTask(task, data.values, recordId);
}

function resetPredictionView() {
  state.result = null;
  els.summaryPanel.innerHTML = `
    <div class="prediction-empty">
      <span class="empty-icon" aria-hidden="true">
        <svg viewBox="0 0 24 24"><path d="M8 5.5v13l10-6.5-10-6.5Z"/></svg>
      </span>
      <div>
        <strong>Il risultato apparirà qui</strong>
        <p>Completa i campi richiesti e seleziona “Calcola predizione”. Nessun valore è precompilato; i missing ammessi seguono l’imputazione indicata accanto ai campi.</p>
      </div>
    </div>
  `;
  els.forcePlot.innerHTML = "<div class='chart-empty'>La spiegazione locale sarà disponibile dopo una predizione valida.</div>";
  els.shapTable.innerHTML = "<div class='table-empty'>Nessuna predizione disponibile.</div>";
  els.featureTable.innerHTML = "<div class='table-empty'>I predittori saranno mostrati dopo la predizione.</div>";
}

async function loadTask(taskId) {
  try {
    const previousRecordId = els.recordSelect?.value || "Manuale";
    state.task = state.metadata.tasks.find((x) => x.id === taskId);
    if (!state.task) throw new Error(`Task non trovato: ${taskId}`);
    renderModelSelect();
    renderRecordSelect();
    const recordId = selectAvailableRecord(previousRecordId);
    state.values = await loadValuesForRecord(currentTask(), recordId);
    renderModelNotes();
    renderInputs();
    resetPredictionView();
    loadShapSummary();
  } catch (err) {
    showToast(err.message);
  }
}

async function loadModel(modelId) {
  try {
    const previousRecordId = els.recordSelect.value;
    const options = modelOptionsForTask(state.task);
    state.model = options.find((x) => x.model_id === modelId);
    if (!state.model) throw new Error(`Modello non trovato: ${modelId}`);
    renderRecordSelect();
    const recordId = selectAvailableRecord(previousRecordId);
    state.values = await loadValuesForRecord(currentTask(), recordId);
    renderModelNotes();
    renderInputs();
    resetPredictionView();
    loadShapSummary();
  } catch (err) {
    showToast(err.message);
  }
}

async function loadRecord(recordId) {
  try {
    const task = currentTask();
    state.values = await loadValuesForRecord(task, recordId);
    renderInputs();
    renderModelNotes();
    resetPredictionView();
  } catch (err) {
    showToast(err.message);
  }
}

async function predict() {
  const task = currentTask();
  const requestVersion = ++state.predictionRequestVersion;
  const requestKey = `${task.id}:${task.model_id}`;
  try {
    setLoading(true);
    const data = await predictApi({
      task_id: task.id,
      model_id: task.model_id,
      record_id: els.recordSelect.value,
      values: state.values,
    });
    if (requestVersion !== state.predictionRequestVersion) return;
    if (`${currentTask()?.id}:${currentTask()?.model_id}` !== requestKey) return;
    state.result = data;
    state.values = valuesForTask(task, data.values || state.values, els.recordSelect.value);
    renderSummary(data);
    renderForcePlot(data.shap || []);
    renderTables(data);
    renderInputs();
    const warnings = Array.isArray(data.warnings) ? data.warnings : data.warnings ? [data.warnings] : [];
    if (warnings.length) showToast(warnings.join(" "));
  } catch (err) {
    if (requestVersion !== state.predictionRequestVersion) return;
    if (`${currentTask()?.id}:${currentTask()?.model_id}` !== requestKey) return;
    showToast(err.message);
  } finally {
    if (requestVersion === state.predictionRequestVersion) setLoading(false);
  }
}

async function init() {
  try {
    setLoading(true);
    state.metadata = await api("/api/metadata");
    if (state.metadata.research_only !== true || state.metadata.contains_patient_records !== false) {
      throw new Error("Artefatto incompatibile con i requisiti research-only/privacy.");
    }
    renderTaskSelect();
    await loadTask(els.taskSelect.value);
  } catch (err) {
    showToast(err.message);
    els.forcePlot.textContent = "Errore durante il caricamento.";
  } finally {
    setLoading(false);
  }
}

els.taskSelect.addEventListener("change", () => loadTask(els.taskSelect.value));
els.modelSelect.addEventListener("change", () => loadModel(els.modelSelect.value));
els.recordSelect.addEventListener("change", () => loadRecord(els.recordSelect.value));
els.predictionForm.addEventListener("submit", (event) => {
  event.preventDefault();
  predict();
});

document.querySelectorAll(".nav-item").forEach((item) => {
  item.addEventListener("click", () => {
    document.querySelectorAll(".nav-item").forEach((link) => {
      link.classList.remove("active");
      link.removeAttribute("aria-current");
    });
    item.classList.add("active");
    item.setAttribute("aria-current", "page");
  });
});

init();
