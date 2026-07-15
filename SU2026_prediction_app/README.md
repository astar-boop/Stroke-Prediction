# SU 2026 prediction app

This app is a research prototype and is not intended for clinical use.

## Required run configuration

Set `SU2026_RUN_DIR` to one immutable analysis-run directory containing:

- `outputs/su2026_analysis_ready.csv`
- the `su2026_ml_*.csv` files used to select features, hyperparameters, and reference performance

No dataset or output directory is selected implicitly.

Alternatively, set both `SU2026_ANALYSIS_DATA` and `SU2026_ML_OUTPUT_DIR`.

Optional settings:

- `SU2026_ARTIFACT_PATH`: alternate `.rds` artifact location
- `SU2026_MRS3M_RESULTS_DIR`: alternate validated three-month mRS results directory
- `SU2026_MRS3M_SOURCE_RDS`: source RDS validated by the three-month mRS run; required when `SU2026_RUN_DIR` is not used
- `SU2026_MODEL_SEED`: positive integer; default `20260704`
- `SU2026_SELECTION_FREQUENCY_MIN`: outer-fold feature-consensus threshold in `(0, 1]`; default `0.5`
- `SU2026_REBUILD_ARTIFACT=1`: explicitly rebuild a missing or stale artifact

## Build and launch

From the project root, after configuring the run:

```sh
Rscript SU2026_prediction_app/train_prediction_models.R
Rscript -e 'shiny::runApp("SU2026_prediction_app")'
```

The standalone local web interface uses the same environment and artifact:

```sh
python3 SU2026_prediction_web/server.py
```

## Three-month mRS task

The default task predicts the probability of an unfavorable three-month outcome (`mRS 3–6` versus `0–2`) at the 24-hour landmark. It uses five prespecified inputs: age, sex, pre-event mRS, admission NIHSS, and 24-hour NIHSS. The LASSO refit is the default because it had the lowest point-estimate nested-CV Brier score; elastic net had the highest point-estimate ROC-AUC without demonstrated superiority, and all nine evaluated algorithms can be selected in the interface.

This task was derived from 82 patients with 22 unfavorable outcomes. It is available only after the 24-hour NIHSS measurement, warns when an input is outside the observed training range, and remains a research-only refit without external validation.

Startup fails closed when the dataset, analytical outputs, training code, or artifact manifest do not match. Before writing and again while loading, an uncompressed serialized-payload gate rejects patient-ID patterns and the record-level fields `record_id`, `excel_row`, or `source_row`, including values hidden in model closure environments. The artifact contains compact model parameters and aggregate synthetic SHAP backgrounds only; it does not contain patient records or record identifiers.
