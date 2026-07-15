required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "janitor", "tibble",
  "glmnet", "rpart", "partykit", "ranger", "xgboost", "nnet"
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop(
    "Pacchetti R mancanti: ", paste(missing_packages, collapse = ", "),
    ". Installarli nell'ambiente del progetto prima di costruire l'artefatto."
  )
}
invisible(lapply(required_packages, require, character.only = TRUE))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    candidate <- sub("^--file=", "", file_arg[[1]])
    if (!identical(candidate, "-") && basename(candidate) == "train_prediction_models.R") {
      return(dirname(normalizePath(candidate, mustWork = TRUE)))
    }
  }
  frames <- sys.frames()
  ofiles <- vapply(frames, function(f) f$ofile %||% NA_character_, character(1))
  ofiles <- ofiles[!is.na(ofiles) & basename(ofiles) == "train_prediction_models.R"]
  if (length(ofiles) > 0) return(dirname(normalizePath(tail(ofiles, 1))))
  candidates <- c(getwd(), file.path(getwd(), "SU2026_prediction_app"))
  hit <- candidates[file.exists(file.path(candidates, "train_prediction_models.R"))]
  if (length(hit) > 0) return(normalizePath(hit[[1]], mustWork = TRUE))
  stop("Impossibile determinare la directory di SU2026_prediction_app.")
}

app_dir <- script_dir()
root_dir <- normalizePath(file.path(app_dir, ".."))
artifact_format_version <- 4L

read_seed <- function() {
  raw <- Sys.getenv("SU2026_MODEL_SEED", "20260704")
  value <- suppressWarnings(as.integer(raw))
  if (length(value) != 1 || is.na(value) || value <= 0) {
    stop("SU2026_MODEL_SEED deve essere un intero positivo.")
  }
  value
}

read_selection_frequency_min <- function() {
  raw <- Sys.getenv("SU2026_SELECTION_FREQUENCY_MIN", "0.5")
  value <- suppressWarnings(as.numeric(raw))
  if (length(value) != 1 || !is.finite(value) || value <= 0 || value > 1) {
    stop("SU2026_SELECTION_FREQUENCY_MIN deve essere compreso nell'intervallo (0, 1].")
  }
  value
}

resolve_build_config <- function() {
  run_dir_raw <- Sys.getenv("SU2026_RUN_DIR", "")
  data_raw <- Sys.getenv("SU2026_ANALYSIS_DATA", "")
  output_raw <- Sys.getenv("SU2026_ML_OUTPUT_DIR", "")
  audit_raw <- Sys.getenv("SU2026_ARTIFACT_AUDIT_DIR", "")
  mrs3m_results_raw <- Sys.getenv(
    "SU2026_MRS3M_RESULTS_DIR",
    file.path(root_dir, "output", "latex", "Neurological_Sciences_mRS3m_focus", "analysis_outputs")
  )
  mrs3m_source_rds_raw <- Sys.getenv("SU2026_MRS3M_SOURCE_RDS", "")

  if (!nzchar(run_dir_raw) && (!nzchar(data_raw) || !nzchar(output_raw))) {
    stop(
      "Configurazione mancante. Impostare SU2026_RUN_DIR, oppure entrambe ",
      "SU2026_ANALYSIS_DATA e SU2026_ML_OUTPUT_DIR. Nessun dataset di fallback viene utilizzato."
    )
  }

  run_dir <- if (nzchar(run_dir_raw)) normalizePath(run_dir_raw, mustWork = TRUE) else NA_character_
  data_path <- if (nzchar(data_raw)) data_raw else file.path(run_dir, "outputs", "su2026_analysis_ready.csv")
  output_path <- if (nzchar(output_raw)) output_raw else file.path(run_dir, "outputs")
  data_path <- normalizePath(data_path, mustWork = TRUE)
  output_path <- normalizePath(output_path, mustWork = TRUE)
  if (!dir.exists(output_path)) stop("Directory output ML non trovata: ", output_path)

  audit_path <- if (nzchar(audit_raw)) audit_raw else output_path
  if (!dir.exists(audit_path)) {
    stop("Directory audit artefatto non trovata: ", audit_path)
  }
  audit_path <- normalizePath(audit_path, mustWork = TRUE)

  mrs3m_results_path <- normalizePath(mrs3m_results_raw, mustWork = TRUE)
  mrs3m_primary_24h_dir <- normalizePath(
    file.path(mrs3m_results_path, "primary_24h"),
    mustWork = TRUE
  )
  mrs3m_xai_24h_dir <- normalizePath(
    file.path(mrs3m_results_path, "xai_24h"),
    mustWork = TRUE
  )
  mrs3m_source_rds <- if (nzchar(mrs3m_source_rds_raw)) {
    normalizePath(mrs3m_source_rds_raw, mustWork = TRUE)
  } else if (!is.na(run_dir)) {
    normalizePath(file.path(run_dir, "outputs", "su2026_analysis_ready.rds"), mustWork = TRUE)
  } else {
    stop(
      "SU2026_MRS3M_SOURCE_RDS e richiesto quando il trainer viene configurato senza SU2026_RUN_DIR."
    )
  }

  # A binary-risk release is usable only after the complete pipeline and its
  # final validation gate have both succeeded. This prevents an artifact from
  # being trained against copied or partially regenerated ML outputs.
  binary_recode_checks <- file.path(output_path, "su2026_binary_risk_recode_checks.csv")
  is_binary_risk_release <- (!is.na(run_dir) && grepl("binary_risk_0_1", basename(run_dir), fixed = TRUE)) ||
    file.exists(binary_recode_checks)
  if (is_binary_risk_release) {
    if (is.na(run_dir)) {
      stop("SU2026_RUN_DIR e richiesto per una release binary-risk.")
    }
    status_path <- file.path(run_dir, "RUN_STATUS")
    validation_path <- file.path(output_path, "binary_risk_pipeline_validation.csv")
    if (!file.exists(status_path) || trimws(readLines(status_path, warn = FALSE)[1]) != "COMPLETE_BINARY_RISK_ANALYSES") {
      stop("Release binary-risk non completa: RUN_STATUS non e COMPLETE_BINARY_RISK_ANALYSES.")
    }
    if (!file.exists(validation_path)) {
      stop("Validazione finale binary-risk mancante: ", validation_path)
    }
    if (!file.exists(binary_recode_checks)) {
      stop("Controlli di ricodifica binary-risk mancanti: ", binary_recode_checks)
    }
    release_validation <- read.csv(validation_path, stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(release_validation) == 0L || !all(release_validation$status == "PASS")) {
      stop("La validazione finale binary-risk non e interamente PASS.")
    }
    recode_validation <- read.csv(binary_recode_checks, stringsAsFactors = FALSE, check.names = FALSE)
    if (nrow(recode_validation) == 0L || !all(recode_validation$status == "PASS")) {
      stop("I controlli di ricodifica binary-risk non sono interamente PASS.")
    }
  }

  artifact_raw <- Sys.getenv(
    "SU2026_ARTIFACT_PATH",
    file.path(app_dir, "su2026_prediction_artifacts.rds")
  )
  artifact_parent <- dirname(artifact_raw)
  if (!dir.exists(artifact_parent)) {
    stop("Directory dell'artefatto non trovata: ", artifact_parent)
  }

  list(
    run_dir = run_dir,
    run_id = if (!is.na(run_dir)) basename(run_dir) else "explicit-inputs",
    data_path = data_path,
    output_dir = output_path,
    audit_dir = audit_path,
    mrs3m_results_dir = mrs3m_results_path,
    mrs3m_primary_24h_dir = mrs3m_primary_24h_dir,
    mrs3m_xai_24h_dir = mrs3m_xai_24h_dir,
    mrs3m_source_rds = mrs3m_source_rds,
    artifact_path = normalizePath(artifact_raw, mustWork = FALSE),
    seed = read_seed(),
    selection_frequency_min = read_selection_frequency_min()
  )
}

build_config <- resolve_build_config()
analysis_data_path <- build_config$data_path
output_dir <- build_config$output_dir
mrs3m_primary_24h_dir <- build_config$mrs3m_primary_24h_dir
mrs3m_xai_24h_dir <- build_config$mrs3m_xai_24h_dir
artifact_path <- build_config$artifact_path
model_seed <- build_config$seed
set.seed(model_seed)

file_md5 <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  unname(tools::md5sum(path)[[1]])
}

artifact_privacy_hits <- function(artifact) {
  # Uncompressed serialization exposes character values, field names,
  # row names and captured closure environments to one fail-closed scan.
  payload <- serialize(artifact, connection = NULL, ascii = FALSE, xdr = TRUE)
  bytes <- as.integer(payload)
  printable <- bytes %in% c(9L, 10L, 13L, 32L:126L)
  bytes[!printable] <- 10L
  text <- rawToChar(as.raw(bytes))

  extract_matches <- function(pattern) {
    positions <- gregexpr(pattern, text, perl = TRUE)[[1]]
    if (length(positions) == 1L && positions[[1]] == -1L) return(character())
    unique(regmatches(text, list(positions))[[1]])
  }

  list(
    patient_ids = extract_matches("(?i)SU[0-9]{3}(?![0-9])"),
    sensitive_fields = extract_matches("(?i)(record_id|excel_row|source_row)")
  )
}

assert_artifact_privacy_gate <- function(artifact) {
  hits <- artifact_privacy_hits(artifact)
  if (length(hits$patient_ids) > 0L || length(hits$sensitive_fields) > 0L) {
    details <- c(
      if (length(hits$patient_ids) > 0L) {
        paste0("patient ID: ", paste(utils::head(hits$patient_ids, 5L), collapse = ", "))
      },
      if (length(hits$sensitive_fields) > 0L) {
        paste0("campi sensibili: ", paste(hits$sensitive_fields, collapse = ", "))
      }
    )
    stop(
      "Controllo privacy serializzato fallito (", paste(details, collapse = "; "), ").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

stable_seed <- function(...) {
  key <- paste(..., collapse = "::")
  offset <- sum(utf8ToInt(enc2utf8(key)) * seq_along(utf8ToInt(enc2utf8(key))))
  as.integer((model_seed + offset) %% (.Machine$integer.max - 1L) + 1L)
}

make_reproducible_foldid <- function(y, nfolds, seed, stratified = FALSE) {
  n <- length(y)
  if (n < nfolds || nfolds < 3) stop("Numero di fold non valido per il campione disponibile.")
  set.seed(seed)
  foldid <- integer(n)
  groups <- if (stratified) split(seq_len(n), as.character(y), drop = TRUE) else list(seq_len(n))
  for (idx in groups) {
    if (length(idx) > 1) idx <- idx[sample.int(length(idx))]
    foldid[idx] <- rep(seq_len(nfolds), length.out = length(idx))
  }
  if (any(foldid == 0L)) stop("Assegnazione fold incompleta.")
  foldid
}

num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
txt <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- ""
  stringr::str_squish(y)
}
sigmoid <- function(x) 1 / (1 + exp(-x))
mode_value <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return("missing_or_not_applicable")
  names(sort(table(x), decreasing = TRUE))[1]
}
clean_label <- function(x) {
  stringr::str_to_sentence(gsub("_", " ", x, fixed = TRUE))
}

normalize_mtici_grade <- function(evt, mtici) {
  evt_num <- num(evt)
  mtici_txt <- tolower(txt(mtici))
  mtici_txt <- stringr::str_replace_all(mtici_txt, "\\s+", "")
  dplyr::case_when(
    is.na(evt_num) | evt_num != 1 ~ "Non EVT / non applicabile",
    mtici_txt %in% c("", "na", "missing") ~ "EVT - mTICI missing",
    mtici_txt %in% c("0", "1", "2", "2a", "2b", "2c", "3") ~ paste0("mTICI ", mtici_txt),
    TRUE ~ "EVT - mTICI altro"
  )
}

derive_successful_recanalization <- function(evt, mtici_grade) {
  evt_num <- num(evt)
  mtici_txt <- tolower(txt(mtici_grade))
  mtici_txt <- stringr::str_replace(mtici_txt, "^mtici\\s*", "")
  mtici_txt <- stringr::str_replace_all(mtici_txt, "\\s+", "")
  dplyr::case_when(
    is.na(evt_num) | evt_num != 1 ~ "Non EVT / non applicabile",
    mtici_txt %in% c("2b", "2c", "3") ~ "si",
    mtici_txt %in% c("0", "1", "2", "2a") ~ "no",
    TRUE ~ "EVT - mTICI missing"
  )
}

ivt_timing_vars <- c("door_to_needle_min_recalc", "onset_to_needle_min_recalc")
evt_timing_vars <- c(
  "onset_to_groin_min_recalc",
  "door_to_groin_min_recalc",
  "groin_to_tici_min_recalc",
  "onset_to_tici_min_recalc"
)

apply_conditional_timing_rules <- function(data) {
  out <- data
  if ("ivt_0_no_1_si" %in% names(out)) {
    for (nm in intersect(ivt_timing_vars, names(out))) {
      out[[nm]] <- dplyr::if_else(num(out[["ivt_0_no_1_si"]]) == 1, num(out[[nm]]), NA_real_)
    }
  }
  if ("evt_si_no" %in% names(out)) {
    for (nm in intersect(evt_timing_vars, names(out))) {
      out[[nm]] <- dplyr::if_else(num(out[["evt_si_no"]]) == 1, num(out[[nm]]), NA_real_)
    }
  }
  out
}

assert_conditional_timing_rules <- function(data) {
  if ("ivt_0_no_1_si" %in% names(data)) {
    for (nm in intersect(ivt_timing_vars, names(data))) {
      stopifnot(all(is.na(data[[nm]]) | num(data[["ivt_0_no_1_si"]]) == 1))
    }
  }
  if ("evt_si_no" %in% names(data)) {
    for (nm in intersect(evt_timing_vars, names(data))) {
      stopifnot(all(is.na(data[[nm]]) | num(data[["evt_si_no"]]) == 1))
    }
  }
  TRUE
}

feature_to_raw_any <- function(feature, variables) {
  if (feature %in% variables) return(feature)
  if (stringr::str_detect(feature, "_missing_flagmissing$")) {
    base <- stringr::str_replace(feature, "_missing_flagmissing$", "")
    if (base %in% variables) return(base)
  }
  candidates <- variables[order(nchar(variables), decreasing = TRUE)]
  hit <- candidates[vapply(candidates, function(x) startsWith(feature, x), logical(1))]
  if (length(hit) > 0) return(hit[[1]])
  feature
}

load_training_data <- function() {
  extension <- tolower(tools::file_ext(analysis_data_path))
  raw <- if (extension == "csv") {
    read.csv(analysis_data_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
  } else if (extension %in% c("xlsx", "xls")) {
    readxl::read_excel(analysis_data_path, sheet = "su2026_analysis_ready", guess_max = 10000)
  } else {
    stop("Formato dataset non supportato: ", extension, ". Usare CSV, XLSX o XLS.")
  }
  names(raw) <- janitor::make_clean_names(names(raw))
  required_columns <- c(
    "analysis_eligible", "ischemico_0_emorragico_1_tia_2_altro_3",
    "m_rs_dimissione", "m_rs_a_3_mesi_0_6", "age_years_analysis",
    "nihss_dimissione_numeric", "neuro_score_dimissione_type",
    "nihss_24h_numeric", "neuro_score_24h_type", "los_days_recalc",
    "ivt_0_no_1_si", "evt_si_no", "excel_row", "record_id"
  )
  missing_columns <- setdiff(required_columns, names(raw))
  if (length(missing_columns) > 0) {
    stop("Il dataset configurato non contiene le colonne richieste: ", paste(missing_columns, collapse = ", "))
  }
  raw <- apply_conditional_timing_rules(raw)
  assert_conditional_timing_rules(raw)

  ml_df <- raw %>%
    mutate(
      ischemic = num(ischemico_0_emorragico_1_tia_2_altro_3) == 0,
      outcome_mrs_discharge = num(m_rs_dimissione),
      outcome_mrs_gt2 = dplyr::case_when(
        !is.na(num(m_rs_dimissione)) & num(m_rs_dimissione) > 2 ~ 1L,
        !is.na(num(m_rs_dimissione)) & num(m_rs_dimissione) <= 2 ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    filter(ischemic, !is.na(outcome_mrs_gt2)) %>%
    mutate(outcome = outcome_mrs_gt2)

  mrs3m_24h_source_df <- raw %>%
    mutate(
      eligible = !is.na(analysis_eligible) & as.logical(analysis_eligible),
      ischemic = num(ischemico_0_emorragico_1_tia_2_altro_3) == 0,
      outcome_mrs3m = num(m_rs_a_3_mesi_0_6),
      outcome_mrs3m_valid = is.finite(outcome_mrs3m) &
        outcome_mrs3m %in% 0:6 &
        abs(outcome_mrs3m - round(outcome_mrs3m)) < 1e-8,
      outcome = as.integer(outcome_mrs3m >= 3),
      age_years_analysis = num(age_years_analysis),
      sesso_m_0_f_1 = num(sesso_m_0_f_1),
      m_rs_pre_evento_0_5 = num(m_rs_pre_evento_0_5),
      nihss_allingresso_numeric = num(nihss_allingresso_numeric),
      nihss_24h_numeric = num(nihss_24h_numeric)
    ) %>%
    filter(
      eligible,
      !is.na(ischemic) & ischemic,
      outcome_mrs3m_valid,
      is.finite(nihss_24h_numeric)
    ) %>%
    select(
      excel_row, record_id, outcome,
      age_years_analysis, sesso_m_0_f_1, m_rs_pre_evento_0_5,
      nihss_allingresso_numeric, nihss_24h_numeric
    )
  if (nrow(mrs3m_24h_source_df) != 82L || sum(mrs3m_24h_source_df$outcome) != 22L) {
    stop(
      "Riconciliazione coorte mRS a 3 mesi fallita: attesi 82 pazienti e 22 outcome sfavorevoli."
    )
  }

  nihss_source_df <- raw %>%
    mutate(
      ischemic = num(ischemico_0_emorragico_1_tia_2_altro_3) == 0,
      outcome_nihss_discharge = num(nihss_dimissione_numeric),
      neuro_score_discharge_type = toupper(txt(neuro_score_dimissione_type)),
      outcome_nihss_gt5 = dplyr::case_when(
        !is.na(outcome_nihss_discharge) & outcome_nihss_discharge > 5 ~ 1L,
        !is.na(outcome_nihss_discharge) & outcome_nihss_discharge <= 5 ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    filter(ischemic, neuro_score_discharge_type == "NIHSS", !is.na(outcome_nihss_discharge), !is.na(outcome_nihss_gt5))

  nihss_24h_target_source_df <- raw %>%
    mutate(
      ischemic = num(ischemico_0_emorragico_1_tia_2_altro_3) == 0,
      neuro_score_24h_type = toupper(txt(neuro_score_24h_type)),
      outcome_nihss_24h = num(nihss_24h_numeric)
    ) %>%
    filter(ischemic, neuro_score_24h_type == "NIHSS", !is.na(outcome_nihss_24h))

  los_source_df <- raw %>%
    mutate(
      outcome_los = num(los_days_recalc),
      outcome_los_gt7 = dplyr::case_when(
        !is.na(outcome_los) & outcome_los > 7 ~ 1L,
        !is.na(outcome_los) & outcome_los <= 7 ~ 0L,
        TRUE ~ NA_integer_
      ),
      m_tici_grade = factor(
        normalize_mtici_grade(evt_si_no, m_tici_0_3),
        levels = c(
          "Non EVT / non applicabile",
          "EVT - mTICI missing",
          "mTICI 0",
          "mTICI 1",
          "mTICI 2a",
          "mTICI 2",
          "mTICI 2b",
          "mTICI 2c",
          "mTICI 3",
          "EVT - mTICI altro"
        )
      ),
      successful_recanalization = factor(
        derive_successful_recanalization(evt_si_no, m_tici_grade),
        levels = c("Non EVT / non applicabile", "EVT - mTICI missing", "no", "si")
      )
    ) %>%
    filter(!is.na(outcome_los)) %>%
    mutate(outcome = outcome_los)

  explicit_exclude <- c(
    "excel_row", "record_id", "outcome_mrs_discharge", "outcome_mrs_gt2", "outcome", "outcome_label", "ischemic",
    "m_rs_dimissione", "nihss_dimissione", "nihss_dimissione_numeric", "gcs_dimissione",
    "neuro_score_dimissione_type", "neuro_score_dimissione_note",
    "modalita_dimissione_0_casa_1_casa_adi_2_cod56_3_cod60_4_altro_5_decesso",
    "data_di_dimissione", "discharge_normalized", "durata_degenza", "los_days_recalc",
    "date_normalization_flag", "los_discrepancy_days",
    "m_rs_a_3_mesi_0_6", "interventi_chirurgici", "ricorrenza_tia_stroke_si_no",
    "data_ricorrenza", "emorragia_cerebrale_si_no", "data_emorragia", "major_bleeding_si_no",
    "data_major_bleeding", "ima_si_no", "neoriscontro_fa_si_no",
    "antiaggregante_3", "anticoagulante_3", "doppia_antiaggregazione_si_no",
    "anticoagulante_antiaggregante_si_no", "antipertensivo_si_no", "ipolipemizzante_si_no",
    "ipoglicemizzante_orale_si_no", "insulina_rapida_si_no", "insulina_lenta_si_no",
    "antiepilettico_si_no", "complicanze_intraospedaliere",
    "nihss_allingresso", "nihss_24h", "nihss_24h_numeric", "gcs_24h",
    "neuro_score_24h_type", "neuro_score_24h_note"
  )
  pattern_exclude <- stringr::str_subset(
    names(ml_df),
    paste(c(
      "^data_", "normalized$", "_time_flag$", "_note$", "raw$",
      "^onset$", "^arrivo_ps$", "^ora_", "_time_min$", "_to_.*_min$", "^tempo_",
      "altro_specificare", "descrivere", "specificare_quale", "farmaci_intraprocedurali"
    ), collapse = "|")
  )

  column_missing_rate <- function(x) mean(is.na(x) | txt(x) == "")
  column_type <- function(x) {
    nonmissing <- txt(x)
    nonmissing <- nonmissing[nonmissing != ""]
    if (length(nonmissing) == 0) return("empty")
    xn <- num(nonmissing)
    if (mean(!is.na(xn)) >= 0.95) "numeric" else "categorical"
  }

  feature_inventory <- tibble::tibble(
    variable = names(ml_df),
    missing_rate = vapply(ml_df, column_missing_rate, numeric(1)),
    inferred_type = vapply(ml_df, column_type, character(1)),
    n_distinct_nonmissing = vapply(ml_df, function(x) dplyr::n_distinct(txt(x)[txt(x) != ""]), integer(1)),
    availability = dplyr::case_when(
      variable %in% c(
        "eta",
        "sesso_m_0_f_1",
        "ipertensione_arteriosa",
        "diabete",
        "pregresso_ictus_tia",
        "pregresso_ima",
        "fumo_0_no_1_attivo_2_ex",
        "dislipidemia",
        "trombofilia",
        "abuso_di_alcol",
        "uso_di_sostanze_stupefacenti",
        "fibrillazione_atriale",
        "ateromasia_carotidea_50_percent",
        "ateromasia_vertebrale",
        "patologia_neoplastica",
        "utilizzo_contraccettivi_orali",
        "familirita_per_ictus",
        "antiaggregante",
        "anticoagulante",
        "antipertensivo",
        "ipolipemizzante",
        "ipoglicemizzante_orale",
        "insulina",
        "m_rs_pre_evento_0_5",
        "nihss_allingresso_numeric",
        "gcs_allingresso",
        "wake_up_stroke_si_no",
        "modalita_di_arrivo_in_ps_118_1_autopresentazione_2_altro_3",
        "onset_to_door_min_recalc",
        "door_to_imaging_min_recalc",
        "door_to_needle_min_recalc",
        "onset_to_needle_min_recalc",
        "onset_to_groin_min_recalc",
        "door_to_groin_min_recalc",
        "groin_to_tici_min_recalc",
        "onset_to_tici_min_recalc",
        "ivt_0_no_1_si",
        "evt_si_no"
      ) ~ "available_in_acute_pathway_before_24h",
      variable == "nihss_24h_numeric" ~ "available_only_for_24h_sensitivity",
      TRUE ~ "not_available_at_admission_or_post_baseline"
    ),
    initial_status = dplyr::case_when(
      variable %in% c("excel_row", "record_id") ~ "audit_id",
      availability == "available_in_acute_pathway_before_24h" ~ "candidate_acute_pre_24h",
      availability == "available_only_for_24h_sensitivity" ~ "candidate_24h_only",
      variable %in% explicit_exclude ~ "excluded_target_or_post_outcome",
      variable %in% pattern_exclude ~ "excluded_leakage_or_raw_text_time",
      missing_rate > 0.90 ~ "excluded_missing_gt90",
      inferred_type == "categorical" & n_distinct_nonmissing > 15 ~ "excluded_high_cardinality_text",
      TRUE ~ "excluded_unavailable_in_acute_pre_24h_or_post_baseline"
    )
  )

  # Mirror the controlled-code overrides used by the verified nested-CV
  # workflow. Re-inferring these numeric-looking variables as continuous here
  # would make the deployment refit incompatible with the released dummy
  # feature names and could silently omit selected predictors.
  categorical_predictor_overrides <- c(
    "sesso_m_0_f_1",
    "ipertensione_arteriosa",
    "diabete",
    "pregresso_ictus_tia",
    "pregresso_ima",
    "fumo_0_no_1_attivo_2_ex",
    "dislipidemia",
    "trombofilia",
    "abuso_di_alcol",
    "uso_di_sostanze_stupefacenti",
    "fibrillazione_atriale",
    "ateromasia_carotidea_50_percent",
    "ateromasia_vertebrale",
    "patologia_neoplastica",
    "utilizzo_contraccettivi_orali",
    "familirita_per_ictus",
    "antiaggregante",
    "anticoagulante",
    "antipertensivo",
    "ipolipemizzante",
    "ipoglicemizzante_orale",
    "insulina",
    "wake_up_stroke_si_no",
    "modalita_di_arrivo_in_ps_118_1_autopresentazione_2_altro_3",
    "ivt_0_no_1_si",
    "evt_si_no"
  )
  missing_controlled_predictors <- setdiff(categorical_predictor_overrides, feature_inventory$variable)
  if (length(missing_controlled_predictors) > 0L) {
    stop(
      "Predittori controllati mancanti nel dataset artefatto: ",
      paste(missing_controlled_predictors, collapse = ", ")
    )
  }
  feature_inventory <- feature_inventory %>%
    mutate(
      inferred_type = dplyr::if_else(
        variable %in% categorical_predictor_overrides,
        "categorical",
        inferred_type
      )
    )
  if (!all(
    feature_inventory$inferred_type[
      match(categorical_predictor_overrides, feature_inventory$variable)
    ] == "categorical"
  )) {
    stop("Override categorici non applicati nel trainer artefatto.")
  }

  released_inventory_path <- file.path(output_dir, "su2026_ml_feature_inventory.csv")
  if (file.exists(released_inventory_path)) {
    released_inventory <- read.csv(
      released_inventory_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    released_types <- released_inventory$inferred_type[
      match(categorical_predictor_overrides, released_inventory$variable)
    ]
    if (anyNA(released_types) || !all(released_types == "categorical")) {
      stop("Il trainer e l'inventario ML rilasciato non concordano sui predittori categorici.")
    }
  }

  los_allowlist <- c(
    "ischemico_0_emorragico_1_tia_2_altro_3",
    "eta",
    "sesso_m_0_f_1",
    "ipertensione_arteriosa",
    "diabete",
    "pregresso_ictus_tia",
    "pregresso_ima",
    "fumo_0_no_1_attivo_2_ex",
    "dislipidemia",
    "trombofilia",
    "abuso_di_alcol",
    "uso_di_sostanze_stupefacenti",
    "fibrillazione_atriale",
    "ateromasia_carotidea_50_percent",
    "ateromasia_vertebrale",
    "patologia_neoplastica",
    "utilizzo_contraccettivi_orali",
    "familirita_per_ictus",
    "antiaggregante",
    "anticoagulante",
    "antipertensivo",
    "ipolipemizzante",
    "ipoglicemizzante_orale",
    "insulina",
    "m_rs_pre_evento_0_5",
    "nihss_allingresso_numeric",
    "gcs_allingresso",
    "wake_up_stroke_si_no",
    "modalita_di_arrivo_in_ps_118_1_autopresentazione_2_altro_3",
    "ivt_0_no_1_si",
    "evt_si_no",
    "onset_to_door_min_recalc",
    "door_to_imaging_min_recalc",
    "door_to_needle_min_recalc",
    "onset_to_needle_min_recalc",
    "onset_to_groin_min_recalc",
    "door_to_groin_min_recalc",
    "groin_to_tici_min_recalc",
    "onset_to_tici_min_recalc",
    "m_tici_grade",
    "successful_recanalization"
  )

  los_feature_inventory <- tibble::tibble(
    variable = names(los_source_df),
    missing_rate = vapply(los_source_df, column_missing_rate, numeric(1)),
    inferred_type = vapply(los_source_df, column_type, character(1)),
    n_distinct_nonmissing = vapply(los_source_df, function(x) dplyr::n_distinct(txt(x)[txt(x) != ""]), integer(1)),
    availability = dplyr::case_when(
      variable %in% los_allowlist ~ "available_for_los_model",
      variable == "nihss_24h_numeric" ~ "available_only_for_los_24h_sensitivity",
      TRUE ~ "not_available_for_los_model"
    ),
    initial_status = dplyr::case_when(
      variable %in% c("excel_row", "record_id") ~ "audit_id",
      variable %in% los_allowlist ~ "candidate_los",
      variable == "nihss_24h_numeric" ~ "candidate_los_24h_only",
      variable %in% explicit_exclude ~ "excluded_target_or_post_outcome",
      variable %in% pattern_exclude ~ "excluded_leakage_or_raw_text_time",
      missing_rate > 0.90 ~ "excluded_missing_gt90",
      inferred_type == "categorical" & n_distinct_nonmissing > 15 ~ "excluded_high_cardinality_text",
      TRUE ~ "excluded_unavailable_or_post_baseline_for_los"
    )
  ) %>%
    mutate(
      inferred_type = dplyr::if_else(
        variable %in% c("ischemico_0_emorragico_1_tia_2_altro_3", "m_tici_grade", "successful_recanalization"),
        "categorical",
        inferred_type
      )
    )

  predictor_cols <- feature_inventory %>%
    filter(initial_status == "candidate_acute_pre_24h") %>%
    pull(variable)
  predictor_cols_24h <- unique(c(predictor_cols, "nihss_24h_numeric"))
  los_predictor_cols <- los_feature_inventory %>%
    filter(initial_status == "candidate_los") %>%
    pull(variable)
  los_predictor_cols_24h <- los_feature_inventory %>%
    filter(initial_status %in% c("candidate_los", "candidate_los_24h_only")) %>%
    pull(variable)

  prepare_features_with_inventory <- function(data, predictor_cols, inventory) {
    out <- data %>% select(excel_row, record_id, outcome, all_of(predictor_cols))
    for (nm in predictor_cols) {
      typ <- inventory$inferred_type[match(nm, inventory$variable)]
      if (typ == "numeric") {
        out[[nm]] <- num(out[[nm]])
        miss_rate <- mean(is.na(out[[nm]]))
        if (miss_rate > 0.05) {
          out[[paste0(nm, "_missing_flag")]] <- factor(if_else(is.na(out[[nm]]), "missing", "observed"), levels = c("observed", "missing"))
        }
      } else {
        y <- txt(out[[nm]])
        y[y == ""] <- "missing_or_not_applicable"
        out[[nm]] <- factor(y)
      }
    }
    out
  }

  prepare_features <- function(data, predictor_cols) {
    prepare_features_with_inventory(data, predictor_cols, feature_inventory)
  }

  prepare_features_los <- function(data, predictor_cols) {
    prepare_features_with_inventory(data, predictor_cols, los_feature_inventory)
  }

  analysis_df <- prepare_features(ml_df, predictor_cols)
  regression_df <- analysis_df %>% mutate(outcome = ml_df$outcome_mrs_discharge)
  nihss_class_df <- prepare_features(nihss_source_df %>% mutate(outcome = outcome_nihss_gt5), predictor_cols)
  nihss_regression_df <- prepare_features(nihss_source_df %>% mutate(outcome = outcome_nihss_discharge), predictor_cols)
  nihss_24h_target_regression_df <- prepare_features(nihss_24h_target_source_df %>% mutate(outcome = outcome_nihss_24h), predictor_cols)
  los_regression_df <- prepare_features_los(los_source_df, los_predictor_cols)
  los_classification_df <- prepare_features_los(los_source_df %>% mutate(outcome = outcome_los_gt7), los_predictor_cols)
  los_regression_24h_df <- prepare_features_los(los_source_df, los_predictor_cols_24h)
  los_classification_24h_df <- prepare_features_los(los_source_df %>% mutate(outcome = outcome_los_gt7), los_predictor_cols_24h)
  analysis_24h_df <- prepare_features(ml_df, predictor_cols_24h)
  regression_24h_df <- analysis_24h_df %>% mutate(outcome = ml_df$outcome_mrs_discharge)
  nihss_class_24h_df <- prepare_features(nihss_source_df %>% mutate(outcome = outcome_nihss_gt5), predictor_cols_24h)
  nihss_regression_24h_df <- prepare_features(nihss_source_df %>% mutate(outcome = outcome_nihss_discharge), predictor_cols_24h)

  utils::write.csv(
    dplyr::bind_rows(
      feature_inventory %>%
        mutate(
          audit_scope = "mrs_nihss",
          acute_pre_24h_model_allowed = initial_status == "candidate_acute_pre_24h",
          plus_24h_model_allowed = initial_status %in% c("candidate_acute_pre_24h", "candidate_24h_only"),
          los_model_allowed = FALSE,
          los_plus_24h_model_allowed = FALSE
        ),
      los_feature_inventory %>%
        mutate(
          audit_scope = "los",
          acute_pre_24h_model_allowed = FALSE,
          plus_24h_model_allowed = FALSE,
          los_model_allowed = initial_status == "candidate_los",
          los_plus_24h_model_allowed = initial_status %in% c("candidate_los", "candidate_los_24h_only")
        )
    ),
    file.path(build_config$audit_dir, "su2026_prediction_app_leakage_audit.csv")
    ,
    row.names = FALSE
  )

  list(
    raw = raw,
    feature_inventory = feature_inventory,
    predictor_cols = predictor_cols,
    predictor_cols_24h = predictor_cols_24h,
    datasets = list(
      mrs_class_baseline = analysis_df,
      mrs_class_24h = analysis_24h_df,
      nihss_class_baseline = nihss_class_df,
      nihss_class_24h = nihss_class_24h_df,
      mrs_reg_baseline = regression_df,
      mrs_reg_24h = regression_24h_df,
      nihss_reg_baseline = nihss_regression_df,
      nihss_reg_24h = nihss_regression_24h_df,
      nihss_24h_target_regression = nihss_24h_target_regression_df,
      los_regression = los_regression_df,
      los_regression_24h = los_regression_24h_df,
      los_classification = los_classification_df,
      los_classification_24h = los_classification_24h_df,
      mrs3m_class_24h = mrs3m_24h_source_df
    )
  )
}

feature_to_raw <- function(feature, prepared_cols) {
  if (feature %in% prepared_cols) return(feature)
  if (stringr::str_detect(feature, "_missing_flagmissing$")) {
    return(stringr::str_replace(feature, "_missing_flagmissing$", ""))
  }
  candidates <- prepared_cols[order(nchar(prepared_cols), decreasing = TRUE)]
  hit <- candidates[vapply(candidates, function(x) startsWith(feature, x), logical(1))]
  if (length(hit) > 0) return(hit[[1]])
  feature
}

clinical_input_bounds <- function(variable, values) {
  observed <- values[is.finite(values)]
  observed_min <- if (length(observed) > 0) min(observed) else NA_real_
  observed_max <- if (length(observed) > 0) max(observed) else NA_real_
  if (variable %in% c("eta", "age_years_analysis")) return(c(0, 120))
  if (stringr::str_detect(variable, "nihss")) return(c(0, 42))
  if (stringr::str_detect(variable, "(^|_)gcs(_|$)")) return(c(3, 15))
  if (identical(variable, "m_rs_pre_evento_0_5")) return(c(0, 5))
  if (stringr::str_detect(variable, "m_rs")) return(c(0, 6))
  if (stringr::str_detect(variable, "_min_recalc$")) return(c(0, 10080))
  if (length(observed) > 0 && all(observed %in% c(0, 1))) return(c(0, 1))
  c(observed_min, observed_max)
}

make_aggregate_background <- function(x) {
  x <- as.matrix(x)
  if (ncol(x) == 0) return(x[0, , drop = FALSE])
  center <- colMeans(x, na.rm = TRUE)
  center[!is.finite(center)] <- 0
  spread <- apply(x, 2, stats::sd, na.rm = TRUE)
  spread[!is.finite(spread)] <- 0
  lower <- apply(x, 2, min, na.rm = TRUE)
  upper <- apply(x, 2, max, na.rm = TRUE)
  lower[!is.finite(lower)] <- center[!is.finite(lower)]
  upper[!is.finite(upper)] <- center[!is.finite(upper)]
  z <- c(-1, -0.5, 0, 0.5, 1)
  background <- vapply(z, function(multiplier) center + multiplier * spread, numeric(length(center)))
  background <- t(background)
  background <- sweep(background, 2, lower, pmax)
  background <- sweep(background, 2, upper, pmin)
  colnames(background) <- colnames(x)
  rownames(background) <- paste0("aggregate_", seq_len(nrow(background)))
  background
}

make_training_design <- function(
    df,
    raw_inputs,
    selected_features,
    feature_inventory,
    required_design_features = selected_features,
    reference_predictors = raw_inputs) {
  prepared_cols <- names(df)
  raw_inputs <- intersect(raw_inputs, prepared_cols)
  missing_flag_cols <- unique(stringr::str_replace(
    selected_features[
      stringr::str_detect(selected_features, "_missing_flag(?:missing|observed)$")
    ],
    "(?:missing|observed)$",
    ""
  ))
  reference_predictors <- intersect(reference_predictors, prepared_cols)
  prepared_predictors <- unique(c(
    reference_predictors,
    raw_inputs,
    intersect(missing_flag_cols, prepared_cols)
  ))

  train <- df %>% select(all_of(prepared_predictors))
  variable_info <- list()
  for (nm in names(train)) {
    if (is.numeric(train[[nm]])) {
      med <- stats::median(train[[nm]], na.rm = TRUE)
      if (is.na(med)) med <- 0
      bounds <- clinical_input_bounds(nm, train[[nm]])
      missing_observed <- any(is.na(train[[nm]]))
      observed_values <- sort(unique(train[[nm]][is.finite(train[[nm]])]))
      allowed_values <- if (
        length(observed_values) > 0 && length(observed_values) <= 12 &&
          all(abs(observed_values - round(observed_values)) < 1e-8)
      ) observed_values else NULL
      train[[nm]][is.na(train[[nm]])] <- med
      variable_info[[nm]] <- list(
        variable = nm,
        label = clean_label(nm),
        type = "numeric",
        median = med,
        min = bounds[[1]],
        max = bounds[[2]],
        allowed_values = allowed_values,
        allow_missing = missing_observed
      )
    } else {
      values <- txt(train[[nm]])
      values[values == ""] <- "missing_or_not_applicable"
      input_levels <- unique(values)
      design_levels <- unique(c(input_levels, "unseen_in_training"))
      train[[nm]] <- factor(values, levels = design_levels)
      variable_info[[nm]] <- list(
        variable = nm,
        label = clean_label(nm),
        type = "categorical",
        levels = input_levels,
        design_levels = design_levels,
        default = mode_value(values),
        allow_missing = "missing_or_not_applicable" %in% input_levels
      )
    }
  }

  single_level_factors <- names(train)[vapply(train, function(x) {
    is.factor(x) && length(levels(droplevels(x))) < 2
  }, logical(1))]
  if (length(single_level_factors) > 0) {
    train <- train %>% select(-all_of(single_level_factors))
    variable_info[single_level_factors] <- NULL
    prepared_predictors <- setdiff(prepared_predictors, single_level_factors)
    raw_inputs <- setdiff(raw_inputs, single_level_factors)
  }

  mm <- stats::model.matrix(~ . - 1, data = train)
  keep_design_column <- vapply(seq_len(ncol(mm)), function(column_index) {
    column <- mm[, column_index]
    all(is.finite(column)) && stats::sd(column, na.rm = TRUE) > 0
  }, logical(1))
  mm <- mm[, keep_design_column, drop = FALSE]
  missing_required_features <- setdiff(required_design_features, colnames(mm))
  if (length(missing_required_features) > 0L) {
    stop(
      "Feature selezionate nella release assenti dal design di refit: ",
      paste(missing_required_features, collapse = ", "),
      call. = FALSE
    )
  }
  selected_design_features <- intersect(selected_features, colnames(mm))
  if (length(selected_design_features) == 0) {
    stop("Nessuna feature selezionata e disponibile nel design di refit.", call. = FALSE)
  }
  x <- mm[, selected_design_features, drop = FALSE]
  col_means <- colMeans(x, na.rm = TRUE)

  list(
    x = x,
    raw_inputs = raw_inputs,
    prepared_predictors = prepared_predictors,
    selected_design_features = selected_design_features,
    design_columns = colnames(mm),
    variable_info = variable_info,
    column_means = col_means,
    aggregate_background = make_aggregate_background(x),
    training_prepared = train
  )
}

choose_tree_config <- function(file, model_id) {
  hp <- read.csv(file, stringsAsFactors = FALSE)
  hp <- hp[hp$model_id == model_id, , drop = FALSE]
  pick_mode <- function(x, default) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(default)
    as.numeric(names(sort(table(x), decreasing = TRUE))[1])
  }
  list(
    cp = pick_mode(hp$cp, 0.005),
    maxdepth = pick_mode(hp$maxdepth, 4),
    minbucket = pick_mode(hp$minbucket, 5)
  )
}

choose_model_config <- function(file, model_id) {
  hp <- read.csv(file, stringsAsFactors = FALSE)
  hp <- hp[hp$model_id == model_id, , drop = FALSE]
  pick_mode <- function(x, default) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(default)
    as.numeric(names(sort(table(x), decreasing = TRUE))[1])
  }
  list(
    alpha = pick_mode(hp$alpha, NA_real_),
    cp = pick_mode(hp$cp, 0.005),
    maxdepth = pick_mode(hp$maxdepth, 4),
    minbucket = pick_mode(hp$minbucket, 5),
    mtry_frac = pick_mode(hp$mtry_frac, 0.70),
    min_node = pick_mode(hp$min_node, 3),
    eta = pick_mode(hp$eta, 0.05),
    nrounds = as.integer(pick_mode(hp$nrounds, 80)),
    nn_size = as.integer(pick_mode(hp$nn_size, 4)),
    decay = pick_mode(hp$decay, 0.001),
    maxit_nn = as.integer(pick_mode(hp$maxit_nn, 300))
  )
}

metric_id_from_summary_metric <- function(summary_metric) {
  dplyr::recode(
    tolower(summary_metric),
    "roc-auc" = "auc",
    "pr-auc" = "pr_auc",
    "rmse" = "rmse",
    "mae" = "mae",
    .default = tolower(summary_metric)
  )
}

metric_higher_is_better <- function(metric_id) {
  metric_id %in% c("auc", "pr_auc", "accuracy", "f1_score", "precision", "ppv", "npv", "r2")
}

metric_display_label <- function(metric_id, fallback = NULL) {
  out <- dplyr::recode(
    metric_id,
    "auc" = "ROC-AUC",
    "pr_auc" = "PR-AUC",
    "accuracy" = "Accuracy",
    "f1_score" = "F1 score",
    "ppv" = "PPV",
    "npv" = "NPV",
    "precision" = "Precision",
    "mae" = "MAE",
    "rmse" = "RMSE",
    "r2" = "R2",
    .default = fallback %||% metric_id
  )
  out
}

model_metrics_table <- function(spec, model_id, model_label) {
  empty <- data.frame(
    metric = character(),
    metric_label = character(),
    estimate = numeric(),
    ci_low = numeric(),
    ci_high = numeric(),
    estimate_ci = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(spec$performance_file)) return(empty)
  path <- file.path(output_dir, spec$performance_file)
  if (!file.exists(path)) return(empty)
  perf <- read.csv(path, stringsAsFactors = FALSE)
  perf <- perf %>%
    filter(model_id == .env$model_id | model_label == .env$model_label) %>%
    distinct(metric, .keep_all = TRUE)
  if (nrow(perf) == 0) return(empty)
  order <- c("auc", "pr_auc", "accuracy", "f1_score", "ppv", "npv", "precision", "mae", "rmse", "r2")
  perf %>%
    mutate(
      metric_order = match(metric, order),
      metric_order = ifelse(is.na(metric_order), length(order) + dplyr::row_number(), metric_order),
      metric_label = metric_display_label(metric, metric_label)
    ) %>%
    arrange(metric_order) %>%
    transmute(
      metric,
      metric_label,
      estimate,
      ci_low,
      ci_high,
      estimate_ci
    ) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

infer_model_kind <- function(model_label, outcome_type) {
  if (outcome_type == "classification") {
    return(dplyr::case_when(
      stringr::str_detect(model_label, "penalizzata") ~ "lasso_classification",
      model_label == "Regressione logistica" ~ "logistic_classification",
      model_label %in% c("Decision tree", "Decision tree condizionale") ~ "ctree_classification",
      model_label == "CART" ~ "cart_classification",
      model_label == "Random Forest" ~ "random_forest_classification",
      model_label == "XGBoost" ~ "xgboost_classification",
      model_label == "Rete neurale" ~ "neural_network_classification",
      TRUE ~ "lasso_classification"
    ))
  }
  dplyr::case_when(
    stringr::str_detect(model_label, "penalizzata") ~ "glmnet_regression",
    model_label == "Regressione lineare" ~ "linear_regression",
    model_label == "CART regressivo" ~ "cart_regression",
    model_label == "Decision tree regressivo" ~ "ctree_regression",
    model_label == "Random Forest regressiva" ~ "random_forest_regression",
    model_label == "XGBoost regressivo" ~ "xgboost_regression",
    model_label == "Rete neurale regressiva" ~ "neural_network_regression",
    TRUE ~ "linear_regression"
  )
}

model_id_from_label <- function(model_label, outcome_type) {
  if (outcome_type == "classification") {
    return(dplyr::case_when(
      model_label == "Regressione logistica" ~ "logistic",
      model_label == "Logistica penalizzata - lasso" ~ "lasso",
      model_label == "Logistica penalizzata - ridge" ~ "ridge",
      model_label == "Logistica penalizzata - elastic net" ~ "elastic_net",
      model_label %in% c("Decision tree", "Decision tree condizionale") ~ "decision_tree",
      model_label == "CART" ~ "cart",
      model_label == "Random Forest" ~ "random_forest",
      model_label == "XGBoost" ~ "xgboost",
      model_label == "Rete neurale" ~ "neural_network",
      TRUE ~ "lasso"
    ))
  }
  dplyr::case_when(
    model_label == "Regressione lineare" ~ "linear_regression",
    model_label == "Regressione penalizzata - lasso" ~ "lasso_regression",
    model_label == "Regressione penalizzata - ridge" ~ "ridge_regression",
    model_label == "Regressione penalizzata - elastic net" ~ "elastic_net_regression",
    model_label == "Decision tree regressivo" ~ "decision_tree_regression",
    model_label == "CART regressivo" ~ "cart_regression",
    model_label == "Random Forest regressiva" ~ "random_forest_regression",
    model_label == "XGBoost regressivo" ~ "xgboost_regression",
    model_label == "Rete neurale regressiva" ~ "neural_network_regression",
    TRUE ~ "linear_regression"
  )
}

compact_model_for_serialization <- function(model, model_kind) {
  if (identical(model_kind, "logistic_classification")) {
    if (is.list(model) && identical(names(model), "coefficients")) return(model)
    return(list(coefficients = stats::coef(model)))
  }
  if (model_kind %in% c("ctree_classification", "ctree_regression")) {
    compact <- partykit::as.simpleparty(model)
    compact$data <- compact$data[0, , drop = FALSE]
    # as.simpleparty() otherwise retains terms and update/trafo closures whose
    # environments capture the complete fit_task_model() frame, including df
    # and prep with record identifiers. predict_task() supplies newdata with
    # the exact prototype names/classes, so these fit-time objects are unused.
    attr(compact$data, "terms") <- NULL
    attr(compact$data, "row.names") <- .set_row_names(0L)
    compact$terms <- NULL
    compact$update <- NULL
    compact$trafo <- NULL
    return(compact)
  }
  if (model_kind %in% c("cart_classification", "cart_regression")) {
    model$where <- NULL
    model$y <- NULL
    if (!is.null(model$terms)) environment(model$terms) <- baseenv()
    return(model)
  }
  if (model_kind %in% c("random_forest_classification", "random_forest_regression")) {
    model$predictions <- NULL
    return(model)
  }
  if (model_kind %in% c("neural_network_classification", "neural_network_regression")) {
    model$fit$fitted.values <- NULL
    model$fit$residuals <- NULL
    return(model)
  }
  model
}

fit_task_model <- function(task_id, spec, prep, best_summary, model_label_override = NULL, model_id_override = NULL) {
  best_row <- best_summary %>%
    filter(task == spec$summary_task, predictor_set == spec$predictor_set, metric == spec$summary_metric) %>%
    dplyr::slice(1)
  if (nrow(best_row) == 0) {
    if (is.null(spec$app_model_label) || is.null(spec$performance_file)) {
      stop("Nessuna riga performance trovata per: ", spec$summary_task, " / ", spec$predictor_set, " / ", spec$summary_metric)
    }
    best_row <- data.frame(
      task = spec$summary_task,
      predictor_set = spec$predictor_set,
      metric = spec$summary_metric,
      best_model = spec$app_model_label,
      estimate_ci = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  performance_note <- "nested CV ricalcolata con whitelist leakage"
  if (!is.null(model_label_override) || !is.null(model_id_override)) {
    if (is.null(spec$performance_file)) {
      stop("performance_file richiesto per selezionare un modello alternativo nel task: ", task_id)
    }
    metric_id <- metric_id_from_summary_metric(spec$summary_metric)
    perf <- read.csv(file.path(output_dir, spec$performance_file), stringsAsFactors = FALSE)
    app_row <- perf %>%
      filter(metric == metric_id) %>%
      {
        if (!is.null(model_id_override)) filter(., model_id == model_id_override) else .
      } %>%
      {
        if (!is.null(model_label_override)) filter(., model_label == model_label_override) else .
      } %>%
      dplyr::slice(1)
    if (nrow(app_row) == 0) {
      stop("Performance non trovata per modello alternativo nel task: ", task_id)
    }
    best_row$best_model <- app_row$model_label[[1]]
    best_row$estimate_ci <- app_row$estimate_ci[[1]]
    best_row$model_id <- app_row$model_id[[1]]
    performance_note <- spec$performance_note %||% performance_note
  } else if (!is.null(spec$app_model_label)) {
    if (is.null(spec$performance_file)) {
      stop("performance_file richiesto quando app_model_label e specificato per task: ", task_id)
    }
    metric_id <- metric_id_from_summary_metric(spec$summary_metric)
    perf <- read.csv(file.path(output_dir, spec$performance_file), stringsAsFactors = FALSE)
    app_row <- perf %>%
      filter(model_label == spec$app_model_label, metric == metric_id) %>%
      dplyr::slice(1)
    if (nrow(app_row) == 0) {
      stop("Performance non trovata per modello app: ", spec$app_model_label, " / ", metric_id)
    }
    best_row$best_model <- spec$app_model_label
    best_row$estimate_ci <- app_row$estimate_ci[[1]]
    best_row$model_id <- app_row$model_id[[1]]
    performance_note <- spec$performance_note %||% performance_note
  }
  best_model <- best_row$best_model[[1]]
  model_kind <- spec$model_kind
  if (!is.null(model_label_override) || !is.null(model_id_override) || model_kind %in% c("auto", "auto_regression", "auto_classification")) {
    model_kind <- infer_model_kind(best_model, spec$outcome_type)
  }
  model_id <- best_row$model_id[[1]] %||% model_id_from_label(best_model, spec$outcome_type)
  metrics <- model_metrics_table(spec, model_id, best_model)
  fs <- read.csv(file.path(output_dir, spec$feature_file), stringsAsFactors = FALSE)
  fs <- fs %>% filter(
    model_label == best_model,
    selection_frequency >= build_config$selection_frequency_min
  )
  if (nrow(fs) == 0) {
    stop(
      "Nessuna feature raggiunge la soglia di consenso ", build_config$selection_frequency_min,
      " per ", task_id, " / ", best_model, "."
    )
  }

  df <- prep$datasets[[spec$dataset]]
  prepared_cols <- setdiff(names(df), c("excel_row", "record_id", "outcome"))
  release_selected_features <- unique(fs$feature)
  selected_features <- release_selected_features

  acute_timing_cols <- c(
    "onset_to_door_min_recalc",
    "door_to_imaging_min_recalc",
    "door_to_needle_min_recalc",
    "onset_to_needle_min_recalc",
    "onset_to_groin_min_recalc",
    "door_to_groin_min_recalc",
    "groin_to_tici_min_recalc",
    "onset_to_tici_min_recalc"
  )
  treatment_context <- intersect(c("ivt_0_no_1_si", "evt_si_no"), prepared_cols)
  selected_raw <- unique(vapply(selected_features, feature_to_raw, character(1), prepared_cols = prepared_cols))
  timing_context <- intersect(acute_timing_cols, selected_raw)
  timing_missing_flags <- paste0(timing_context, "_missing_flagmissing")
  timing_missing_flags <- timing_missing_flags[paste0(timing_context, "_missing_flag") %in% prepared_cols]
  selected_features <- unique(c(selected_features, treatment_context, timing_missing_flags))

  raw_inputs <- unique(vapply(selected_features, feature_to_raw, character(1), prepared_cols = prepared_cols))
  raw_inputs <- intersect(raw_inputs, prepared_cols)
  raw_inputs <- raw_inputs[!stringr::str_detect(raw_inputs, "_missing_flag$")]

  design <- make_training_design(
    df,
    raw_inputs,
    selected_features,
    prep$feature_inventory,
    required_design_features = release_selected_features,
    reference_predictors = prepared_cols
  )
  y <- df$outcome
  x <- design$x
  task_seed <- stable_seed(task_id, model_id, best_model)
  set.seed(task_seed)

  model <- NULL
  safe_names <- make.names(colnames(x), unique = TRUE)
  if (model_kind == "lasso_classification") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    alpha <- cfg$alpha
    if (is.na(alpha)) {
      alpha <- dplyr::case_when(
        model_id == "ridge" ~ 0,
        model_id == "elastic_net" ~ 0.5,
        TRUE ~ 1
      )
    }
    nfolds <- min(5L, min(table(y)))
    if (nfolds < 3L) stop("Classi insufficienti per una CV interna a 3 fold: ", task_id)
    foldid <- make_reproducible_foldid(y, nfolds, task_seed, stratified = TRUE)
    model <- glmnet::cv.glmnet(
      as.matrix(x), as.integer(y), family = "binomial", alpha = alpha,
      foldid = foldid, nfolds = nfolds, maxit = 1000000, parallel = FALSE
    )
  } else if (model_kind == "logistic_classification") {
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- as.integer(y)
    model <- suppressWarnings(stats::glm(y ~ ., data = x_df, family = stats::binomial()))
  } else if (model_kind == "ctree_classification") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- factor(ifelse(as.integer(y) == 1, "positive", "negative"), levels = c("negative", "positive"))
    model <- partykit::ctree(
      y ~ .,
      data = x_df,
      control = partykit::ctree_control(mincriterion = 0.90, minbucket = cfg$minbucket, maxdepth = cfg$maxdepth)
    )
  } else if (model_kind == "cart_classification") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- factor(ifelse(as.integer(y) == 1, "positive", "negative"), levels = c("negative", "positive"))
    model <- rpart::rpart(
      y ~ .,
      data = x_df,
      method = "class",
      control = rpart::rpart.control(cp = cfg$cp, maxdepth = cfg$maxdepth, minbucket = cfg$minbucket)
    )
  } else if (model_kind == "random_forest_classification") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- factor(ifelse(as.integer(y) == 1, "positive", "negative"), levels = c("negative", "positive"))
    model <- ranger::ranger(
      y ~ .,
      data = x_df,
      probability = TRUE,
      num.trees = 400,
      mtry = max(1, min(ncol(x), round(ncol(x) * cfg$mtry_frac))),
      min.node.size = cfg$min_node,
      seed = task_seed,
      num.threads = 1
    )
  } else if (model_kind == "xgboost_classification") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(x), label = as.integer(y))
    pos_weight <- sum(as.integer(y) == 0) / max(sum(as.integer(y) == 1), 1)
    model <- xgboost::xgb.train(
      params = list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        max_depth = cfg$maxdepth,
        eta = cfg$eta,
        min_child_weight = 1,
        subsample = 0.85,
        colsample_bytree = 0.85,
        scale_pos_weight = pos_weight,
        nthread = 1
      ),
      data = dtrain,
      nrounds = cfg$nrounds,
      verbose = 0
    )
  } else if (model_kind == "neural_network_classification") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    center <- colMeans(x, na.rm = TRUE)
    scale <- apply(x, 2, stats::sd, na.rm = TRUE)
    scale[is.na(scale) | scale == 0] <- 1
    x_scaled <- sweep(sweep(as.matrix(x), 2, center, "-"), 2, scale, "/")
    model <- list(
      fit = nnet::nnet(
        x = x_scaled,
        y = as.integer(y),
        size = cfg$nn_size,
        decay = cfg$decay,
        maxit = cfg$maxit_nn,
        entropy = TRUE,
        trace = FALSE,
        MaxNWts = 10000
      ),
      center = center,
      scale = scale
    )
  } else if (model_kind == "glmnet_regression") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    alpha <- cfg$alpha
    if (is.na(alpha)) {
      alpha <- dplyr::case_when(
        model_id == "ridge_regression" ~ 0,
        model_id == "elastic_net_regression" ~ 0.5,
        TRUE ~ 1
      )
    }
    nfolds <- min(5L, nrow(x))
    if (nfolds < 3L) stop("Campione insufficiente per una CV interna a 3 fold: ", task_id)
    foldid <- make_reproducible_foldid(y, nfolds, task_seed, stratified = FALSE)
    model <- glmnet::cv.glmnet(
      as.matrix(x), as.numeric(y), family = "gaussian", alpha = alpha,
      foldid = foldid, nfolds = nfolds, maxit = 1000000, parallel = FALSE
    )
  } else if (model_kind == "linear_regression") {
    fit <- stats::lm.fit(cbind("(Intercept)" = 1, as.matrix(x)), as.numeric(y))
    coefs <- as.numeric(fit$coefficients)
    names(coefs) <- colnames(cbind("(Intercept)" = 1, as.matrix(x)))
    model <- list(coefficients = coefs)
  } else if (model_kind == "cart_regression") {
    cfg <- choose_tree_config(file.path(output_dir, spec$hp_file), "cart_regression")
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- as.numeric(y)
    model <- rpart::rpart(
      y ~ .,
      data = x_df,
      method = "anova",
      control = rpart::rpart.control(cp = cfg$cp, maxdepth = cfg$maxdepth, minbucket = cfg$minbucket)
    )
  } else if (model_kind == "ctree_regression") {
    cfg <- choose_tree_config(file.path(output_dir, spec$hp_file), "decision_tree_regression")
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- as.numeric(y)
    model <- partykit::ctree(
      y ~ .,
      data = x_df,
      control = partykit::ctree_control(mincriterion = 0.90, minbucket = cfg$minbucket, maxdepth = cfg$maxdepth)
    )
  } else if (model_kind == "random_forest_regression") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- as.numeric(y)
    model <- ranger::ranger(
      y ~ .,
      data = x_df,
      num.trees = 400,
      mtry = max(1, min(ncol(x), round(ncol(x) * cfg$mtry_frac))),
      min.node.size = cfg$min_node,
      seed = task_seed,
      num.threads = 1
    )
  } else if (model_kind == "xgboost_regression") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(x), label = as.numeric(y))
    model <- xgboost::xgb.train(
      params = list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        max_depth = cfg$maxdepth,
        eta = cfg$eta,
        min_child_weight = 1,
        subsample = 0.85,
        colsample_bytree = 0.85,
        nthread = 1
      ),
      data = dtrain,
      nrounds = cfg$nrounds,
      verbose = 0
    )
  } else if (model_kind == "neural_network_regression") {
    cfg <- choose_model_config(file.path(output_dir, spec$hp_file), model_id)
    center <- colMeans(x, na.rm = TRUE)
    scale <- apply(x, 2, stats::sd, na.rm = TRUE)
    scale[is.na(scale) | scale == 0] <- 1
    x_scaled <- sweep(sweep(as.matrix(x), 2, center, "-"), 2, scale, "/")
    y_center <- mean(as.numeric(y), na.rm = TRUE)
    y_scale <- stats::sd(as.numeric(y), na.rm = TRUE)
    if (is.na(y_scale) || y_scale == 0) y_scale <- 1
    model <- list(
      fit = nnet::nnet(
        x = x_scaled,
        y = (as.numeric(y) - y_center) / y_scale,
        size = cfg$nn_size,
        decay = cfg$decay,
        maxit = cfg$maxit_nn,
        linout = TRUE,
        trace = FALSE,
        MaxNWts = 10000
      ),
      center = center,
      scale = scale,
      y_center = y_center,
      y_scale = y_scale
    )
  }
  model <- compact_model_for_serialization(model, model_kind)

  task <- list(
    task_id = task_id,
    title = spec$title,
    scenario = spec$display_scenario %||% spec$predictor_set,
    model_label = best_model,
    model_id = model_id,
    model_kind = model_kind,
    outcome_type = spec$outcome_type,
    positive_label = spec$positive_label %||% NA_character_,
    units = spec$units %||% "",
    performance = paste0(
      best_row$estimate_ci[[1]], " (", performance_note,
      "; riferimento CV, refit app non validato indipendentemente)"
    ),
    metrics = metrics,
    model = model,
    safe_names = safe_names,
    raw_inputs = design$raw_inputs,
    prepared_predictors = design$prepared_predictors,
    selected_design_features = design$selected_design_features,
    design_columns = design$design_columns,
    variable_info = design$variable_info,
    column_means = design$column_means,
    background_x = design$aggregate_background,
    cohort_summary = list(
      n = nrow(df),
      outcome_nonmissing = sum(!is.na(y)),
      outcome_mean = mean(as.numeric(y), na.rm = TRUE),
      outcome_sd = stats::sd(as.numeric(y), na.rm = TRUE)
    ),
    training_seed = task_seed,
    selection_frequency_min = build_config$selection_frequency_min,
    deployment_validation = "research_refit_not_independently_validated",
    target_bounds = spec$target_bounds %||% c(-Inf, Inf)
  )
  task
}

fit_task_model_set <- function(task_id, spec, prep, best_summary) {
  if (is.null(spec$performance_file)) {
    task <- fit_task_model(task_id, spec, prep, best_summary)
    task$best_model_id <- task$model_id
    task$best_model_label <- task$model_label
    task$is_best_model <- TRUE
    task$model_options <- list(list(
      model_id = task$model_id,
      model_label = task$model_label,
      performance = task$performance,
      metrics = task$metrics,
      is_best = TRUE
    ))
    return(list(default_task = task, model_tasks = stats::setNames(list(task), task$model_id)))
  }

  metric_id <- metric_id_from_summary_metric(spec$summary_metric)
  perf <- read.csv(file.path(output_dir, spec$performance_file), stringsAsFactors = FALSE)
  perf <- perf %>%
    filter(metric == metric_id) %>%
    distinct(model_id, model_label, .keep_all = TRUE)
  if (nrow(perf) == 0) {
    stop("Nessuna performance disponibile per task: ", task_id, " / ", metric_id)
  }
  perf <- if (metric_higher_is_better(metric_id)) {
    perf %>% arrange(desc(estimate))
  } else {
    perf %>% arrange(estimate)
  }

  best_model_id <- perf$model_id[[1]]
  best_model_label <- perf$model_label[[1]]
  model_tasks <- list()
  option_rows <- list()

  for (i in seq_len(nrow(perf))) {
    row <- perf[i, , drop = FALSE]
    fitted <- tryCatch(
      fit_task_model(
        task_id,
        spec,
        prep,
        best_summary,
        model_label_override = row$model_label[[1]],
        model_id_override = row$model_id[[1]]
      ),
      error = function(e) {
        message("Skipping model for ", task_id, " / ", row$model_label[[1]], ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(fitted)) next
    fitted$best_model_id <- best_model_id
    fitted$best_model_label <- best_model_label
    fitted$is_best_model <- identical(fitted$model_id, best_model_id)
    fitted$option_rank <- i
    model_tasks[[fitted$model_id]] <- fitted
    option_rows[[length(option_rows) + 1]] <- list(
      model_id = fitted$model_id,
      model_label = fitted$model_label,
      performance = fitted$performance,
      metrics = fitted$metrics,
      selected_feature_count = length(fitted$selected_design_features),
      is_best = fitted$is_best_model
    )
  }

  if (length(model_tasks) == 0) {
    stop("Nessun modello addestrabile per task: ", task_id)
  }
  if (!best_model_id %in% names(model_tasks)) {
    stop(
      "Il modello selezionato dalla nested CV non e stato addestrato per il task ",
      task_id,
      ": ",
      best_model_id,
      call. = FALSE
    )
  }
  default_id <- best_model_id
  default_task <- model_tasks[[default_id]]
  default_task$model_options <- option_rows

  for (mid in names(model_tasks)) {
    model_tasks[[mid]]$model_options <- option_rows
  }

  list(default_task = default_task, model_tasks = model_tasks)
}

mrs3m_result_paths <- function() {
  c(
    metrics = file.path(mrs3m_primary_24h_dir, "mrs3m_primary_model_metrics.csv"),
    tuning = file.path(mrs3m_primary_24h_dir, "mrs3m_primary_tuning_summary.csv"),
    run_audit = file.path(mrs3m_primary_24h_dir, "mrs3m_primary_run_audit.csv"),
    validation = file.path(mrs3m_primary_24h_dir, "mrs3m_primary_validation_checks.csv"),
    confusion = file.path(mrs3m_primary_24h_dir, "mrs3m_primary_confusion_summary.csv"),
    paired_deltas = file.path(mrs3m_primary_24h_dir, "mrs3m_primary_paired_model_deltas.csv"),
    xai_refit = file.path(mrs3m_xai_24h_dir, "mrs3m_brier_selected_explanatory_refit.csv"),
    xai_coefficients = file.path(mrs3m_xai_24h_dir, "mrs3m_brier_selected_shap_summary.csv"),
    source_rds = build_config$mrs3m_source_rds
  )
}

validate_mrs3m_results <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop("Output mRS a 3 mesi mancanti: ", paste(missing, collapse = ", "))
  }

  validation <- read.csv(paths[["validation"]], stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(validation) == 0L || !all(validation$status == "PASS")) {
    stop("La validazione del modello mRS a 3 mesi non e interamente PASS.")
  }

  run_audit <- read.csv(paths[["run_audit"]], stringsAsFactors = FALSE, check.names = FALSE)
  audit_value <- function(field) run_audit$value[match(field, run_audit$field)]
  expected_hash <- audit_value("input_rds_md5")
  if (is.na(expected_hash) || !identical(file_md5(paths[["source_rds"]]), expected_hash)) {
    stop("Il dataset configurato non coincide con quello validato per il task mRS a 3 mesi.")
  }
  if (!identical(audit_value("cohort_n"), "82") || !identical(audit_value("events"), "22")) {
    stop("Audit mRS a 3 mesi inatteso: sono richiesti 82 pazienti e 22 eventi.")
  }
  if (!grepl("mRS 3-6 versus 0-2", audit_value("outcome"), fixed = TRUE)) {
    stop("Definizione outcome mRS a 3 mesi non compatibile.")
  }

  metrics <- read.csv(paths[["metrics"]], stringsAsFactors = FALSE, check.names = FALSE)
  expected_models <- c(
    "logistic", "lasso", "ridge", "elastic_net", "decision_tree",
    "cart", "random_forest", "xgboost", "neural_network"
  )
  if (!setequal(metrics$model_id, expected_models) ||
      any(metrics$n != 82L) || any(metrics$events != 22L) ||
      any(!is.finite(metrics$roc_auc)) || any(!is.finite(metrics$brier))) {
    stop("Metriche mRS a 3 mesi incomplete o incoerenti.")
  }
  paired_deltas <- read.csv(paths[["paired_deltas"]], stringsAsFactors = FALSE, check.names = FALSE)
  elastic_net_delta <- paired_deltas[
    paired_deltas$model_id == "elastic_net" & paired_deltas$reference_model_id == "lasso",
    ,
    drop = FALSE
  ]
  if (nrow(elastic_net_delta) != 1L ||
      !is.finite(elastic_net_delta$delta_auc[[1]]) ||
      elastic_net_delta$delta_auc_lower[[1]] >= 0 ||
      elastic_net_delta$delta_auc_upper[[1]] <= 0) {
    stop("Il confronto ROC-AUC Elastic Net versus LASSO non supporta la cautela dichiarata nell'app.")
  }
  invisible(TRUE)
}

mrs3m_model_labels <- c(
  logistic = "Regressione logistica",
  lasso = "Logistica penalizzata - lasso",
  ridge = "Logistica penalizzata - ridge",
  elastic_net = "Logistica penalizzata - elastic net",
  decision_tree = "Decision tree condizionale",
  cart = "CART",
  random_forest = "Random Forest",
  xgboost = "XGBoost",
  neural_network = "Rete neurale"
)

mrs3m_configurations <- function(model_id) {
  if (model_id == "logistic") return(data.frame(config_id = "default"))
  if (model_id %in% c("lasso", "ridge", "elastic_net")) {
    return(data.frame(
      config_id = paste0("lambda_", seq_len(7L)),
      lambda = 10^seq(-3, 0.5, length.out = 7L),
      stringsAsFactors = FALSE
    ))
  }
  if (model_id == "decision_tree") {
    out <- expand.grid(maxdepth = c(2L, 3L), minbucket = c(5L, 10L))
  } else if (model_id == "cart") {
    out <- expand.grid(cp = c(0.005, 0.02), maxdepth = c(2L, 3L), minbucket = c(5L, 10L))
  } else if (model_id == "random_forest") {
    out <- expand.grid(mtry_frac = c(0.5, 1), min_node = c(3L, 8L))
  } else if (model_id == "xgboost") {
    out <- expand.grid(maxdepth = c(1L, 2L), eta = c(0.05, 0.10), nrounds = c(40L, 80L))
  } else if (model_id == "neural_network") {
    out <- expand.grid(size = c(2L, 4L), decay = c(0.001, 0.01))
  } else {
    stop("Modello mRS a 3 mesi sconosciuto: ", model_id)
  }
  out$config_id <- paste0("cfg_", seq_len(nrow(out)))
  out[, c("config_id", setdiff(names(out), "config_id")), drop = FALSE]
}

mrs3m_modal_configuration <- function(tuning, model_id) {
  values <- tuning$selected_config[tuning$model_id == model_id]
  if (length(values) == 0L) stop("Tuning mRS a 3 mesi assente per: ", model_id)
  selected <- names(sort(table(values), decreasing = TRUE))[1]
  grid <- mrs3m_configurations(model_id)
  row <- grid[match(selected, grid$config_id), , drop = FALSE]
  if (nrow(row) != 1L || is.na(row$config_id)) {
    stop("Configurazione mRS a 3 mesi non mappabile: ", model_id, " / ", selected)
  }
  as.list(row[1, , drop = FALSE])
}

mrs3m_metrics_table <- function(metrics_row) {
  metric_spec <- data.frame(
    metric = c(
      "auc", "pr_auc", "brier", "log_loss", "calibration_intercept",
      "calibration_slope", "accuracy", "sensitivity", "specificity", "ppv", "npv", "f1_score"
    ),
    source = c(
      "roc_auc", "pr_auc", "brier", "log_loss", "calibration_intercept",
      "calibration_slope", "accuracy_050", "sensitivity_050", "specificity_050",
      "ppv_050", "npv_050", "f1_050"
    ),
    metric_label = c(
      "ROC-AUC", "PR-AUC", "Brier score", "Log loss", "Intercetta calibrazione",
      "Pendenza calibrazione", "Accuratezza (soglia 0,50)", "Sensibilita (soglia 0,50)",
      "Specificita (soglia 0,50)", "PPV (soglia 0,50)", "NPV (soglia 0,50)",
      "F1 (soglia 0,50)"
    ),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(metric_spec)), function(i) {
    source <- metric_spec$source[[i]]
    estimate <- as.numeric(metrics_row[[source]][[1]])
    lower_name <- paste0(source, "_lower")
    upper_name <- paste0(source, "_upper")
    ci_low <- if (lower_name %in% names(metrics_row)) as.numeric(metrics_row[[lower_name]][[1]]) else NA_real_
    ci_high <- if (upper_name %in% names(metrics_row)) as.numeric(metrics_row[[upper_name]][[1]]) else NA_real_
    estimate_ci <- if (is.finite(ci_low) && is.finite(ci_high)) {
      sprintf("%.3f [%.3f, %.3f]", estimate, ci_low, ci_high)
    } else {
      sprintf("%.3f", estimate)
    }
    data.frame(
      metric = metric_spec$metric[[i]],
      metric_label = metric_spec$metric_label[[i]],
      estimate = estimate,
      ci_low = ci_low,
      ci_high = ci_high,
      estimate_ci = estimate_ci,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

prepare_mrs3m_design <- function(prep) {
  fixed_features <- c(
    "age_years_analysis", "sesso_m_0_f_1", "m_rs_pre_evento_0_5",
    "nihss_allingresso_numeric", "nihss_24h_numeric"
  )
  df <- prep$datasets$mrs3m_class_24h
  design <- make_training_design(
    df = df,
    raw_inputs = fixed_features,
    selected_features = fixed_features,
    feature_inventory = prep$feature_inventory,
    required_design_features = fixed_features,
    reference_predictors = fixed_features
  )
  labels <- c(
    age_years_analysis = "Eta (anni)",
    sesso_m_0_f_1 = "Sesso (0 uomo, 1 donna)",
    m_rs_pre_evento_0_5 = "mRS pre-evento",
    nihss_allingresso_numeric = "NIHSS all'ingresso",
    nihss_24h_numeric = "NIHSS a 24 ore"
  )
  clinical_min <- c(
    age_years_analysis = 0, sesso_m_0_f_1 = 0, m_rs_pre_evento_0_5 = 0,
    nihss_allingresso_numeric = 0, nihss_24h_numeric = 0
  )
  clinical_max <- c(
    age_years_analysis = 120, sesso_m_0_f_1 = 1, m_rs_pre_evento_0_5 = 5,
    nihss_allingresso_numeric = 42, nihss_24h_numeric = 42
  )
  required_inputs <- c("age_years_analysis", "sesso_m_0_f_1", "nihss_24h_numeric")
  for (variable in fixed_features) {
    observed <- df[[variable]][is.finite(df[[variable]])]
    info <- design$variable_info[[variable]]
    info$label <- labels[[variable]]
    info$min <- clinical_min[[variable]]
    info$max <- clinical_max[[variable]]
    info$training_min <- min(observed)
    info$training_max <- max(observed)
    info$warn_outside_training_range <- TRUE
    info$allow_missing <- !(variable %in% required_inputs)
    info$allowed_values <- if (variable == "sesso_m_0_f_1") {
      0:1
    } else if (variable == "m_rs_pre_evento_0_5") {
      0:5
    } else {
      NULL
    }
    design$variable_info[[variable]] <- info
  }
  expected_medians <- c(
    age_years_analysis = 76.5,
    sesso_m_0_f_1 = 1,
    m_rs_pre_evento_0_5 = 0,
    nihss_allingresso_numeric = 6,
    nihss_24h_numeric = 2
  )
  observed_medians <- vapply(
    fixed_features,
    function(variable) design$variable_info[[variable]]$median,
    numeric(1)
  )
  if (max(abs(observed_medians - expected_medians)) > 1e-8) {
    stop("Le imputazioni del refit mRS a 3 mesi non coincidono con il refit XAI pubblicato.")
  }
  design
}

fit_mrs3m_model <- function(model_id, metrics_row, tuning, prep, design) {
  df <- prep$datasets$mrs3m_class_24h
  y <- as.integer(df$outcome)
  x <- as.matrix(design$x)
  safe_names <- make.names(colnames(x), unique = TRUE)
  model_label <- unname(mrs3m_model_labels[[model_id]])
  config <- mrs3m_modal_configuration(tuning, model_id)
  task_seed <- stable_seed("mrs3m_class_24h", model_id, config$config_id)
  set.seed(task_seed)

  if (model_id %in% c("lasso", "ridge", "elastic_net")) {
    alpha <- c(lasso = 1, ridge = 0, elastic_net = 0.5)[[model_id]]
    fit <- glmnet::glmnet(
      x, y, family = "binomial", alpha = alpha,
      lambda = config$lambda, standardize = TRUE, maxit = 1000000L
    )
    coefficients <- as.matrix(stats::coef(fit, s = config$lambda))[, 1]
    model <- list(coefficients = coefficients)
    model_kind <- "logistic_classification"
  } else if (model_id == "logistic") {
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- y
    fit <- suppressWarnings(stats::glm(y ~ ., data = x_df, family = stats::binomial()))
    if (!isTRUE(fit$converged) || any(!is.finite(stats::coef(fit)))) {
      stop("Il refit logistico mRS a 3 mesi non converge.")
    }
    model <- list(coefficients = stats::coef(fit))
    model_kind <- "logistic_classification"
  } else if (model_id == "decision_tree") {
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- factor(ifelse(y == 1, "positive", "negative"), levels = c("negative", "positive"))
    model <- partykit::ctree(
      y ~ ., data = x_df,
      control = partykit::ctree_control(
        mincriterion = 0.90,
        maxdepth = as.integer(config$maxdepth),
        minbucket = as.integer(config$minbucket)
      )
    )
    model_kind <- "ctree_classification"
  } else if (model_id == "cart") {
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- factor(ifelse(y == 1, "positive", "negative"), levels = c("negative", "positive"))
    model <- rpart::rpart(
      y ~ ., data = x_df, method = "class",
      control = rpart::rpart.control(
        cp = config$cp,
        maxdepth = as.integer(config$maxdepth),
        minbucket = as.integer(config$minbucket),
        xval = 0
      )
    )
    model_kind <- "cart_classification"
  } else if (model_id == "random_forest") {
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- safe_names
    x_df$y <- factor(ifelse(y == 1, "positive", "negative"), levels = c("negative", "positive"))
    model <- ranger::ranger(
      y ~ ., data = x_df, probability = TRUE, num.trees = 500,
      mtry = max(1L, min(ncol(x), round(ncol(x) * config$mtry_frac))),
      min.node.size = as.integer(config$min_node),
      seed = task_seed, num.threads = 1
    )
    model_kind <- "random_forest_classification"
  } else if (model_id == "xgboost") {
    model <- xgboost::xgb.train(
      params = list(
        objective = "binary:logistic", eval_metric = "logloss",
        max_depth = as.integer(config$maxdepth), eta = config$eta,
        min_child_weight = 1, subsample = 0.85, colsample_bytree = 0.85,
        scale_pos_weight = sum(y == 0) / sum(y == 1), nthread = 1
      ),
      data = xgboost::xgb.DMatrix(x, label = y),
      nrounds = as.integer(config$nrounds), verbose = 0
    )
    model_kind <- "xgboost_classification"
  } else if (model_id == "neural_network") {
    center <- colMeans(x)
    scale <- apply(x, 2, stats::sd)
    scale[!is.finite(scale) | scale == 0] <- 1
    x_scaled <- sweep(sweep(x, 2, center, "-"), 2, scale, "/")
    fit <- nnet::nnet(
      x = x_scaled, y = y,
      size = as.integer(config$size), decay = config$decay,
      maxit = 1000, entropy = TRUE, trace = FALSE, MaxNWts = 10000
    )
    if (!isTRUE(fit$convergence == 0L) || any(!is.finite(fit$wts))) {
      stop("Il refit della rete neurale mRS a 3 mesi non converge.")
    }
    model <- list(fit = fit, center = center, scale = scale)
    model_kind <- "neural_network_classification"
  } else {
    stop("Modello mRS a 3 mesi non supportato: ", model_id)
  }
  model <- compact_model_for_serialization(model, model_kind)
  metrics <- mrs3m_metrics_table(metrics_row)
  auc <- metrics$estimate[metrics$metric == "auc"]
  auc_low <- metrics$ci_low[metrics$metric == "auc"]
  auc_high <- metrics$ci_high[metrics$metric == "auc"]
  brier <- metrics$estimate[metrics$metric == "brier"]
  brier_low <- metrics$ci_low[metrics$metric == "brier"]
  brier_high <- metrics$ci_high[metrics$metric == "brier"]

  list(
    task_id = "mrs3m_class_24h",
    title = "mRS sfavorevole a 3 mesi (3–6) — landmark 24h",
    scenario = "Landmark a 24 ore: età, sesso, mRS pre-evento, NIHSS ingresso e NIHSS a 24 ore",
    model_label = model_label,
    model_id = model_id,
    model_kind = model_kind,
    outcome_type = "classification",
    positive_label = "mRS 3–6 a 3 mesi (dipendenza o decesso)",
    units = "",
    performance = sprintf(
      "ROC-AUC %.3f [%.3f, %.3f]; Brier %.3f [%.3f, %.3f] (10 x 5-fold nested CV; refit app non validato indipendentemente)",
      auc, auc_low, auc_high, brier, brier_low, brier_high
    ),
    metrics = metrics,
    model = model,
    safe_names = safe_names,
    raw_inputs = design$raw_inputs,
    prepared_predictors = design$prepared_predictors,
    selected_design_features = design$selected_design_features,
    design_columns = design$design_columns,
    variable_info = design$variable_info,
    column_means = design$column_means,
    background_x = design$aggregate_background,
    cohort_summary = list(n = 82L, events = 22L, nonevents = 60L, event_rate = 22 / 82),
    training_seed = task_seed,
    selection_frequency_min = 1,
    deployment_validation = "research_refit_not_independently_validated",
    target_bounds = c(0, 1),
    landmark = "24 ore",
    selection_criterion = paste(
      "LASSO predefinito per il Brier score puntuale più basso;",
      "Elastic Net ha la ROC-AUC puntuale più alta, senza superiorità dimostrata."
    ),
    limitations = paste(
      "Coorte single-center: 82 pazienti e 22 outcome sfavorevoli;",
      "follow-up mRS osservato nel 37,6% degli ischemici; 16/22 eventi erano decessi;",
      "nessuna validazione esterna. Usare solo dopo la misurazione NIHSS a 24 ore.",
      "I contributi SHAP sono associazioni condizionali, non effetti causali;",
      "il segno può riflettere collinearità o soppressione tra NIHSS ingresso e NIHSS a 24 ore."
    ),
    refit_configuration = config$config_id,
    model_origin = "full_cohort_refit_with_modal_nested_cv_hyperparameters"
  )
}

fit_mrs3m_task_model_set <- function(prep, paths) {
  metrics <- read.csv(paths[["metrics"]], stringsAsFactors = FALSE, check.names = FALSE)
  tuning <- read.csv(paths[["tuning"]], stringsAsFactors = FALSE, check.names = FALSE)
  design <- prepare_mrs3m_design(prep)
  metrics <- metrics %>% arrange(brier, desc(roc_auc))
  model_tasks <- list()
  for (i in seq_len(nrow(metrics))) {
    model_id <- metrics$model_id[[i]]
    fitted <- fit_mrs3m_model(model_id, metrics[i, , drop = FALSE], tuning, prep, design)
    fitted$best_model_id <- "lasso"
    fitted$best_model_label <- unname(mrs3m_model_labels[["lasso"]])
    fitted$is_best_model <- identical(model_id, "lasso")
    fitted$option_rank <- i
    fitted$model_badges <- c(
      if (identical(model_id, "lasso")) "Default - miglior Brier",
      if (identical(model_id, "elastic_net")) "ROC-AUC puntuale più alta"
    )
    model_tasks[[model_id]] <- fitted
  }
  if (!all(names(mrs3m_model_labels) %in% names(model_tasks))) {
    stop("Non tutti i nove modelli mRS a 3 mesi sono stati refittati.")
  }

  xai_refit <- read.csv(paths[["xai_refit"]], stringsAsFactors = FALSE, check.names = FALSE)
  xai_coefficients <- read.csv(paths[["xai_coefficients"]], stringsAsFactors = FALSE, check.names = FALSE)
  lasso <- model_tasks[["lasso"]]
  if (!identical(lasso$refit_configuration, xai_refit$configuration[[1]]) ||
      abs(xai_refit$lambda[[1]] - mrs3m_modal_configuration(tuning, "lasso")$lambda) > 1e-12) {
    stop("Il refit LASSO dell'app non usa la configurazione XAI pubblicata.")
  }
  coefficient_map <- c(
    Age = "age_years_analysis",
    Female_sex = "sesso_m_0_f_1",
    Pre_event_mRS = "m_rs_pre_evento_0_5",
    Admission_NIHSS = "nihss_allingresso_numeric",
    NIHSS_24h = "nihss_24h_numeric"
  )
  expected <- xai_coefficients$coefficient[match(names(coefficient_map), xai_coefficients$feature)]
  observed <- lasso$model$coefficients[unname(coefficient_map)]
  if (anyNA(expected) || anyNA(observed) || max(abs(expected - observed)) > 1e-10) {
    stop("I coefficienti LASSO dell'app non coincidono con il refit XAI pubblicato.")
  }
  base_log_odds <- lasso$model$coefficients[["(Intercept)"]] +
    sum(observed * lasso$column_means[unname(coefficient_map)])
  if (abs(base_log_odds - xai_refit$base_log_odds[[1]]) > 1e-10) {
    stop("Il riferimento SHAP LASSO dell'app non coincide con il refit XAI pubblicato.")
  }
  model_tasks[["lasso"]]$refit_validation <- "matched_published_xai_refit"

  option_rows <- lapply(model_tasks, function(task) list(
    model_id = task$model_id,
    model_label = task$model_label,
    performance = task$performance,
    metrics = task$metrics,
    selected_feature_count = length(task$selected_design_features),
    is_best = task$is_best_model,
    model_badges = task$model_badges
  ))
  for (model_id in names(model_tasks)) model_tasks[[model_id]]$model_options <- option_rows
  default_task <- model_tasks[["lasso"]]
  list(default_task = default_task, model_tasks = model_tasks)
}

assert_privacy_safe_task <- function(task) {
  if (!is.null(task$records)) stop("Controllo privacy fallito: record paziente presenti nel task ", task$task_id)
  if (is.null(task$background_x) || nrow(task$background_x) > 5L) {
    stop("Controllo privacy fallito: background non aggregato nel task ", task$task_id)
  }
  kind <- task$model_kind
  model <- task$model
  unsafe <- FALSE
  if (identical(kind, "logistic_classification")) {
    unsafe <- !identical(names(model), "coefficients")
  } else if (kind %in% c("ctree_classification", "ctree_regression")) {
    unsafe <- !inherits(model, "simpleparty") ||
      nrow(model$data) != 0L ||
      length(attr(model$data, "row.names", exact = TRUE)) != 0L ||
      !is.null(attr(model$data, "terms", exact = TRUE)) ||
      !is.null(model$terms) ||
      !is.null(model$update) ||
      !is.null(model$trafo) ||
      !is.null(model$fitted)
  } else if (kind %in% c("cart_classification", "cart_regression")) {
    unsafe <- !is.null(model$y) || !is.null(model$where) || !is.null(model$model)
  } else if (kind %in% c("random_forest_classification", "random_forest_regression")) {
    unsafe <- !is.null(model$predictions)
  } else if (kind %in% c("neural_network_classification", "neural_network_regression")) {
    unsafe <- !is.null(model$fit$fitted.values) || !is.null(model$fit$residuals)
  }
  if (unsafe) stop("Controllo privacy fallito: payload del modello non compatto per ", task$task_id, " / ", task$model_id)
  invisible(TRUE)
}

build_artifacts <- function() {
  prep <- load_training_data()
  mrs3m_paths <- mrs3m_result_paths()
  validate_mrs3m_results(mrs3m_paths)
  best_summary_path <- file.path(output_dir, "su2026_ml_24h_best_predictor_set_summary.csv")
  if (!file.exists(best_summary_path)) stop("Output ML richiesto non trovato: ", best_summary_path)
  best_summary <- read.csv(best_summary_path, stringsAsFactors = FALSE)

  task_specs <- list(
    mrs_class_baseline = list(
      title = "mRS dimissione > 2 - percorso acuto pre-24h",
      dataset = "mrs_class_baseline",
      summary_task = "mRS dimissione > 2",
      predictor_set = "Acuto pre-24h",
      display_scenario = "Percorso acuto pre-24h: anamnesi, NIHSS ingresso, IVT/EVT e timing disponibili",
      summary_metric = "ROC-AUC",
      feature_file = "su2026_ml_feature_selection_frequency.csv",
      hp_file = "su2026_ml_best_hyperparameters.csv",
      performance_file = "su2026_ml_model_performance_ci.csv",
      model_kind = "lasso_classification",
      outcome_type = "classification",
      positive_label = "mRS > 2"
    ),
    mrs_class_24h = list(
      title = "mRS dimissione > 2 - percorso acuto + NIHSS 24h",
      dataset = "mrs_class_24h",
      summary_task = "mRS dimissione > 2",
      predictor_set = "Acuto pre-24h + NIHSS 24h",
      display_scenario = "Percorso acuto pre-24h + NIHSS 24h",
      summary_metric = "ROC-AUC",
      feature_file = "su2026_ml_mrs_24h_classification_feature_selection_frequency.csv",
      hp_file = "su2026_ml_mrs_24h_classification_best_hyperparameters.csv",
      performance_file = "su2026_ml_mrs_24h_classification_performance_ci.csv",
      model_kind = "lasso_classification",
      outcome_type = "classification",
      positive_label = "mRS > 2"
    ),
    nihss_class_baseline = list(
      title = "NIHSS dimissione > 5 - percorso acuto pre-24h",
      dataset = "nihss_class_baseline",
      summary_task = "NIHSS dimissione > 5",
      predictor_set = "Acuto pre-24h",
      display_scenario = "Percorso acuto pre-24h: anamnesi, NIHSS ingresso, IVT/EVT e timing disponibili",
      summary_metric = "ROC-AUC",
      feature_file = "su2026_ml_nihss_classification_feature_selection_frequency.csv",
      hp_file = "su2026_ml_nihss_classification_best_hyperparameters.csv",
      performance_file = "su2026_ml_nihss_classification_performance_ci.csv",
      model_kind = "lasso_classification",
      outcome_type = "classification",
      positive_label = "NIHSS > 5"
    ),
    nihss_class_24h = list(
      title = "NIHSS dimissione > 5 - percorso acuto + NIHSS 24h",
      dataset = "nihss_class_24h",
      summary_task = "NIHSS dimissione > 5",
      predictor_set = "Acuto pre-24h + NIHSS 24h",
      display_scenario = "Percorso acuto pre-24h + NIHSS 24h",
      summary_metric = "ROC-AUC",
      feature_file = "su2026_ml_nihss_24h_classification_feature_selection_frequency.csv",
      hp_file = "su2026_ml_nihss_24h_classification_best_hyperparameters.csv",
      performance_file = "su2026_ml_nihss_24h_classification_performance_ci.csv",
      model_kind = "lasso_classification",
      outcome_type = "classification",
      positive_label = "NIHSS > 5"
    ),
    mrs_reg_baseline = list(
      title = "mRS dimissione 0-6 - percorso acuto pre-24h",
      dataset = "mrs_reg_baseline",
      summary_task = "mRS dimissione 0-6",
      predictor_set = "Acuto pre-24h",
      display_scenario = "Percorso acuto pre-24h: anamnesi, NIHSS ingresso, IVT/EVT e timing disponibili",
      summary_metric = "RMSE",
      feature_file = "su2026_ml_regression_feature_selection_frequency.csv",
      hp_file = "su2026_ml_regression_best_hyperparameters.csv",
      performance_file = "su2026_ml_regression_performance_ci.csv",
      model_kind = "linear_regression",
      outcome_type = "regression",
      units = "punti mRS",
      target_bounds = c(0, 6)
    ),
    mrs_reg_24h = list(
      title = "mRS dimissione 0-6 - percorso acuto + NIHSS 24h",
      dataset = "mrs_reg_24h",
      summary_task = "mRS dimissione 0-6",
      predictor_set = "Acuto pre-24h + NIHSS 24h",
      display_scenario = "Percorso acuto pre-24h + NIHSS 24h",
      summary_metric = "RMSE",
      feature_file = "su2026_ml_mrs_24h_regression_feature_selection_frequency.csv",
      hp_file = "su2026_ml_mrs_24h_regression_best_hyperparameters.csv",
      performance_file = "su2026_ml_mrs_24h_regression_performance_ci.csv",
      model_kind = "cart_regression",
      outcome_type = "regression",
      units = "punti mRS",
      target_bounds = c(0, 6)
    ),
    nihss_reg_baseline = list(
      title = "NIHSS dimissione numerico - percorso acuto pre-24h",
      dataset = "nihss_reg_baseline",
      summary_task = "NIHSS dimissione numerico",
      predictor_set = "Acuto pre-24h",
      display_scenario = "Percorso acuto pre-24h: anamnesi, NIHSS ingresso, IVT/EVT e timing disponibili",
      summary_metric = "RMSE",
      feature_file = "su2026_ml_nihss_regression_feature_selection_frequency.csv",
      hp_file = "su2026_ml_nihss_regression_best_hyperparameters.csv",
      performance_file = "su2026_ml_nihss_regression_performance_ci.csv",
      model_kind = "ctree_regression",
      outcome_type = "regression",
      units = "punti NIHSS",
      target_bounds = c(0, max(prep$datasets$nihss_reg_baseline$outcome, na.rm = TRUE))
    ),
    nihss_reg_24h = list(
      title = "NIHSS dimissione numerico - percorso acuto + NIHSS 24h",
      dataset = "nihss_reg_24h",
      summary_task = "NIHSS dimissione numerico",
      predictor_set = "Acuto pre-24h + NIHSS 24h",
      display_scenario = "Percorso acuto pre-24h + NIHSS 24h",
      summary_metric = "RMSE",
      feature_file = "su2026_ml_nihss_24h_regression_feature_selection_frequency.csv",
      hp_file = "su2026_ml_nihss_24h_regression_best_hyperparameters.csv",
      performance_file = "su2026_ml_nihss_24h_regression_performance_ci.csv",
      model_kind = "linear_regression",
      outcome_type = "regression",
      units = "punti NIHSS",
      target_bounds = c(0, max(prep$datasets$nihss_reg_24h$outcome, na.rm = TRUE))
    ),
    nihss_24h_target_regression = list(
      title = "NIHSS a 24 ore numerico - percorso acuto pre-24h",
      dataset = "nihss_24h_target_regression",
      summary_task = "NIHSS a 24 ore numerico",
      predictor_set = "Acuto pre-24h",
      display_scenario = "Predizione dell'NIHSS a 24 ore usando solo predittori acuti pre-24h",
      summary_metric = "RMSE",
      feature_file = "su2026_ml_nihss_24h_target_regression_feature_selection_frequency.csv",
      hp_file = "su2026_ml_nihss_24h_target_regression_best_hyperparameters.csv",
      performance_file = "su2026_ml_nihss_24h_target_regression_performance_ci.csv",
      app_model_label = "Regressione lineare",
      performance_note = "nested CV; target NIHSS 24h con predittori acuti pre-24h",
      model_kind = "linear_regression",
      outcome_type = "regression",
      units = "punti NIHSS",
      target_bounds = c(0, max(prep$datasets$nihss_24h_target_regression$outcome, na.rm = TRUE))
    ),
    los_classification = list(
      title = "Durata degenza > 7 giorni - modello unico con mTICI",
      dataset = "los_classification",
      summary_task = "Durata degenza > 7",
      predictor_set = "Acuto pre-24h",
      display_scenario = "Coorte LOS: predittori acuti, EVT e mTICI condizionale",
      summary_metric = "ROC-AUC",
      feature_file = "su2026_ml_los_classification_feature_selection_frequency.csv",
      hp_file = "su2026_ml_los_classification_best_hyperparameters.csv",
      performance_file = "su2026_ml_los_classification_performance_ci.csv",
      model_kind = "auto_classification",
      outcome_type = "classification",
      positive_label = "degenza > 7 giorni"
    ),
    los_classification_24h = list(
      title = "Durata degenza > 7 giorni - percorso acuto + NIHSS 24h",
      dataset = "los_classification_24h",
      summary_task = "Durata degenza > 7",
      predictor_set = "Acuto pre-24h + NIHSS 24h",
      display_scenario = "Coorte LOS: predittori acuti, EVT, mTICI condizionale + NIHSS 24h",
      summary_metric = "ROC-AUC",
      feature_file = "su2026_ml_los_24h_classification_feature_selection_frequency.csv",
      hp_file = "su2026_ml_los_24h_classification_best_hyperparameters.csv",
      performance_file = "su2026_ml_los_24h_classification_performance_ci.csv",
      model_kind = "auto_classification",
      outcome_type = "classification",
      positive_label = "degenza > 7 giorni"
    ),
    los_regression = list(
      title = "Durata di degenza in giorni - modello unico con mTICI",
      dataset = "los_regression",
      summary_task = "Durata degenza",
      predictor_set = "Acuto pre-24h",
      display_scenario = "Coorte LOS: predittori acuti, EVT e mTICI condizionale",
      summary_metric = "RMSE",
      feature_file = "su2026_ml_los_regression_feature_selection_frequency.csv",
      hp_file = "su2026_ml_los_regression_best_hyperparameters.csv",
      performance_file = "su2026_ml_los_regression_performance_ci.csv",
      app_model_label = "Random Forest regressiva",
      performance_note = "nested CV; modello non costante selezionato per predizione individuale LOS",
      model_kind = "auto_regression",
      outcome_type = "regression",
      units = "giorni di degenza",
      target_bounds = c(0, max(prep$datasets$los_regression$outcome, na.rm = TRUE))
    ),
    los_regression_24h = list(
      title = "Durata di degenza in giorni - percorso acuto + NIHSS 24h",
      dataset = "los_regression_24h",
      summary_task = "Durata degenza",
      predictor_set = "Acuto pre-24h + NIHSS 24h",
      display_scenario = "Coorte LOS: predittori acuti, EVT, mTICI condizionale + NIHSS 24h",
      summary_metric = "RMSE",
      feature_file = "su2026_ml_los_24h_regression_feature_selection_frequency.csv",
      hp_file = "su2026_ml_los_24h_regression_best_hyperparameters.csv",
      performance_file = "su2026_ml_los_24h_regression_performance_ci.csv",
      app_model_label = "Random Forest regressiva",
      performance_note = "nested CV + NIHSS 24h; modello non costante selezionato per predizione individuale LOS",
      model_kind = "auto_regression",
      outcome_type = "regression",
      units = "giorni di degenza",
      target_bounds = c(0, max(prep$datasets$los_regression_24h$outcome, na.rm = TRUE))
    )
  )

  release_support_inputs <- c(
    file.path(output_dir, "su2026_ml_feature_inventory.csv"),
    file.path(output_dir, "su2026_binary_risk_recode_checks.csv"),
    file.path(output_dir, "binary_risk_pipeline_validation.csv")
  )
  release_support_inputs <- release_support_inputs[file.exists(release_support_inputs)]

  analytical_input_paths <- unique(c(
    best_summary_path,
    file.path(output_dir, vapply(task_specs, `[[`, character(1), "feature_file")),
    file.path(output_dir, vapply(task_specs, `[[`, character(1), "hp_file")),
    file.path(output_dir, vapply(task_specs, `[[`, character(1), "performance_file")),
    release_support_inputs,
    unname(mrs3m_paths)
  ))
  missing_inputs <- analytical_input_paths[!file.exists(analytical_input_paths)]
  if (length(missing_inputs) > 0) {
    stop("Output analitici mancanti per la costruzione dell'artefatto: ", paste(missing_inputs, collapse = ", "))
  }

  fitted_sets <- lapply(names(task_specs), function(id) fit_task_model_set(id, task_specs[[id]], prep, best_summary))
  names(fitted_sets) <- names(task_specs)
  tasks <- lapply(fitted_sets, `[[`, "default_task")
  model_tasks <- lapply(fitted_sets, `[[`, "model_tasks")
  mrs3m_set <- fit_mrs3m_task_model_set(prep, mrs3m_paths)
  tasks$mrs3m_class_24h <- mrs3m_set$default_task
  model_tasks$mrs3m_class_24h <- mrs3m_set$model_tasks
  if (is.null(model_tasks$los_classification[["cart"]])) {
    stop("Il comparatore CART per la classificazione LOS non e stato addestrato; artefatto non salvato.")
  }
  invisible(lapply(unlist(model_tasks, recursive = FALSE), assert_privacy_safe_task))

  training_code_path <- normalizePath(file.path(app_dir, "train_prediction_models.R"), mustWork = TRUE)
  analytical_keys <- make.unique(basename(analytical_input_paths), sep = "__")
  analytical_paths_named <- stats::setNames(as.list(analytical_input_paths), analytical_keys)
  analytical_hashes <- stats::setNames(
    vapply(analytical_input_paths, file_md5, character(1)),
    analytical_keys
  )

  artifacts <- list(
    artifact_format_version = artifact_format_version,
    generated_at = Sys.time(),
    source_workbook = basename(analysis_data_path),
    source_run = build_config$run_id,
    manifest = list(
      artifact_format_version = artifact_format_version,
      run_id = build_config$run_id,
      dataset_path = analysis_data_path,
      dataset_md5 = file_md5(analysis_data_path),
      training_code_path = training_code_path,
      training_code_md5 = file_md5(training_code_path),
      analytical_input_paths = analytical_paths_named,
      analytical_input_md5 = as.list(analytical_hashes),
      model_seed = model_seed,
      selection_frequency_min = build_config$selection_frequency_min,
      contains_patient_records = FALSE,
      privacy_gate_status = "PASS",
      privacy_gate_version = 1L,
      background_type = "aggregate_mean_sd_grid",
      model_payload_policy = "compact_no_training_rows",
      mrs3m_contract = list(
        task_id = "mrs3m_class_24h",
        outcome = "mRS 3-6 versus 0-2 at three months",
        landmark = "24 hours",
        cohort_n = 82L,
        events = 22L,
        default_model_id = "lasso",
        candidate_models = 9L,
        source_rds_md5 = file_md5(build_config$mrs3m_source_rds)
      )
    ),
    tasks = tasks,
    model_tasks = model_tasks,
    notes = c(
      paste0(
        "Predittori di deployment limitati alle feature selezionate in almeno il ",
        round(100 * build_config$selection_frequency_min), "% dei fold esterni."
      ),
      "Whitelist predittori: anamnesi/terapie pre-evento, NIHSS ingresso, onset-to-door, IVT/EVT si/no e timing acuti disponibili prima delle 24h.",
      "Timing condizionali: door/onset-to-needle sono valorizzabili solo con IVT; groin/TICI sono valorizzabili solo con EVT. Altrimenti entrano come NA con relativo flag di missing/non applicabilita quando selezionato.",
      "Per LOS, mTICI e ricanalizzazione efficace sono incluse come variabili condizionali su EVT; nei non EVT assumono il livello non applicabile.",
      "Le varianti + NIHSS 24h aggiungono la rivalutazione neurologica a 24 ore e sono disponibili solo dopo quella misurazione.",
      "Controlli/imaging a 24h, TOAST, complicanze, passaggi, stent, accesso, terapie post-evento, dimissione e follow-up esclusi per leakage o non disponibilita temporale nei task non LOS.",
      "Le performance CV mostrate nell'app derivano dalla nested CV ricalcolata con whitelist leakage.",
      "Nuovo task mRS a 3 mesi: probabilita di mRS 3-6 (dipendenza o decesso) al landmark di 24 ore, con cinque predittori prespecificati.",
      "Per il task mRS a 3 mesi il LASSO e il default per il Brier score puntuale piu basso; Elastic Net ha la ROC-AUC puntuale piu alta, senza superiorita dimostrata.",
      "Il task mRS a 3 mesi deriva da 82 pazienti con 22 eventi; il follow-up mRS era osservato nel 37,6% degli ischemici e manca validazione esterna.",
      "Tempi originali esclusi: sono ammessi solo *_min_recalc.",
      "SHAP: decomposizione esatta per modelli lineari/logistici; stima permutation-SHAP per alberi.",
      "Artefatto destinato esclusivamente alla ricerca: non validato per decisioni cliniche individuali.",
      "Nessun record paziente e nessun identificativo sono serializzati; il background SHAP contiene soltanto profili aggregati sintetici."
    )
  )
  assert_artifact_privacy_gate(artifacts)
  temporary_artifact <- tempfile(pattern = "su2026_artifact_", tmpdir = dirname(artifact_path), fileext = ".rds")
  on.exit(unlink(temporary_artifact), add = TRUE)
  saveRDS(artifacts, temporary_artifact)
  persisted_artifact <- readRDS(temporary_artifact)
  assert_artifact_privacy_gate(persisted_artifact)
  if (!file.rename(temporary_artifact, artifact_path)) {
    stop("Impossibile salvare atomicamente l'artefatto: ", artifact_path)
  }
  artifacts
}

if (sys.nframe() == 0) {
  artifacts <- build_artifacts()
  cat("Saved prediction artifacts:", artifact_path, "\n")
  cat("Tasks:", paste(names(artifacts$tasks), collapse = ", "), "\n")
}
