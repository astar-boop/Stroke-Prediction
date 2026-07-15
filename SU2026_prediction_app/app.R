required_packages <- c("shiny", "DT", "ggplot2", "dplyr", "stringr", "glmnet", "rpart", "partykit", "ranger", "xgboost", "nnet")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop(
    "Pacchetti R mancanti: ", paste(missing_packages, collapse = ", "),
    ". Installarli nell'ambiente del progetto prima di avviare l'app."
  )
}
invisible(lapply(required_packages, require, character.only = TRUE))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
app_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    candidate <- sub("^--file=", "", file_arg[[1]])
    if (basename(candidate) == "app.R") {
      candidate <- normalizePath(candidate, mustWork = TRUE)
      return(dirname(candidate))
    }
  }
  frames <- sys.frames()
  ofiles <- vapply(frames, function(frame) frame$ofile %||% NA_character_, character(1))
  ofiles <- ofiles[!is.na(ofiles) & basename(ofiles) == "app.R"]
  if (length(ofiles) > 0) return(dirname(normalizePath(tail(ofiles, 1), mustWork = TRUE)))
  candidates <- c(getwd(), file.path(getwd(), "SU2026_prediction_app"))
  hit <- candidates[
    file.exists(file.path(candidates, "app.R")) &
      file.exists(file.path(candidates, "train_prediction_models.R"))
  ]
  if (length(hit) > 0) return(normalizePath(hit[[1]], mustWork = TRUE))
  stop("Impossibile determinare la directory di SU2026_prediction_app.")
}
app_dir <- app_script_dir()
artifact_format_version <- 4L
artifact_path <- normalizePath(
  Sys.getenv("SU2026_ARTIFACT_PATH", file.path(app_dir, "su2026_prediction_artifacts.rds")),
  mustWork = FALSE
)
portable_artifact <- tolower(Sys.getenv("SU2026_PORTABLE_ARTIFACT", "false")) %in% c("1", "true", "yes")

configured_dataset_path <- function() {
  explicit <- Sys.getenv("SU2026_ANALYSIS_DATA", "")
  run_dir <- Sys.getenv("SU2026_RUN_DIR", "")
  if (!nzchar(explicit) && !nzchar(run_dir)) {
    stop(
      "Configurazione mancante: impostare SU2026_RUN_DIR oppure SU2026_ANALYSIS_DATA. ",
      "L'app non usa più il dataset nella cartella outputs come fallback."
    )
  }
  candidate <- if (nzchar(explicit)) explicit else file.path(run_dir, "outputs", "su2026_analysis_ready.csv")
  normalizePath(candidate, mustWork = TRUE)
}

current_dataset_path <- if (portable_artifact) NULL else configured_dataset_path()
training_code_path <- normalizePath(file.path(app_dir, "train_prediction_models.R"), mustWork = TRUE)
file_md5 <- function(path) unname(tools::md5sum(normalizePath(path, mustWork = TRUE))[[1]])

artifact_privacy_hits <- function(artifact) {
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

artifact_validation_errors <- function(artifact) {
  errors <- character()
  if (!identical(artifact$artifact_format_version, artifact_format_version)) {
    errors <- c(errors, "versione/formato artefatto incompatibile")
  }
  manifest <- artifact$manifest
  if (is.null(manifest)) return(c(errors, "manifest assente"))
  if (!identical(manifest$artifact_format_version, artifact_format_version)) {
    errors <- c(errors, "versione manifest incompatibile")
  }
  if (!portable_artifact) {
    manifest_dataset <- tryCatch(normalizePath(manifest$dataset_path, mustWork = TRUE), error = function(e) NA_character_)
    if (is.na(manifest_dataset)) {
      errors <- c(errors, "dataset registrato nel manifest non disponibile")
    } else {
      if (!identical(manifest_dataset, current_dataset_path)) {
        errors <- c(errors, "dataset configurato diverso da quello usato per l'artefatto")
      }
      if (!identical(file_md5(manifest_dataset), manifest$dataset_md5)) {
        errors <- c(errors, "hash del dataset modificato")
      }
    }
  }
  if (!identical(file_md5(training_code_path), manifest$training_code_md5)) {
    errors <- c(errors, "codice di training modificato")
  }
  analytical_paths <- unlist(manifest$analytical_input_paths, use.names = TRUE)
  analytical_hashes <- unlist(manifest$analytical_input_md5, use.names = TRUE)
  if (length(analytical_paths) == 0 || length(analytical_hashes) == 0) {
    errors <- c(errors, "hash degli output analitici assenti")
  } else if (!portable_artifact) {
    for (label in names(analytical_paths)) {
      path <- analytical_paths[[label]]
      if (!file.exists(path)) {
        errors <- c(errors, paste0("output analitico mancante: ", label))
      } else if (is.null(analytical_hashes[[label]]) || !identical(file_md5(path), analytical_hashes[[label]])) {
        errors <- c(errors, paste0("output analitico modificato: ", label))
      }
    }
  }
  all_tasks <- unlist(artifact$model_tasks %||% list(), recursive = FALSE)
  if (any(vapply(all_tasks, function(task) !is.null(task$records), logical(1)))) {
    errors <- c(errors, "l'artefatto contiene record paziente")
  }
  if (any(vapply(all_tasks, function(task) is.null(task$background_x) || nrow(task$background_x) > 5L, logical(1)))) {
    errors <- c(errors, "background SHAP non aggregato")
  }
  if (any(vapply(
    all_tasks,
    function(task) !identical(task$deployment_validation, "research_refit_not_independently_validated"),
    logical(1)
  ))) {
    errors <- c(errors, "stato di validazione deployment non dichiarato")
  }
  if (!identical(manifest$contains_patient_records, FALSE)) {
    errors <- c(errors, "garanzia privacy del manifest assente")
  }
  if (!identical(manifest$model_payload_policy, "compact_no_training_rows")) {
    errors <- c(errors, "policy di minimizzazione dei modelli assente")
  }
  if (!identical(manifest$privacy_gate_status, "PASS") || !identical(manifest$privacy_gate_version, 1L)) {
    errors <- c(errors, "gate privacy serializzato assente o incompatibile")
  }
  privacy_hits <- tryCatch(artifact_privacy_hits(artifact), error = function(e) e)
  if (inherits(privacy_hits, "error")) {
    errors <- c(errors, paste0("gate privacy serializzato non eseguibile: ", conditionMessage(privacy_hits)))
  } else if (length(privacy_hits$patient_ids) > 0L || length(privacy_hits$sensitive_fields) > 0L) {
    errors <- c(errors, "gate privacy serializzato: identificativi o campi record-level rilevati")
  }
  if (is.null(manifest$selection_frequency_min) || !is.finite(manifest$selection_frequency_min)) {
    errors <- c(errors, "soglia di consenso feature assente")
  }
  mrs3m_contract <- manifest$mrs3m_contract
  mrs3m_task <- artifact$tasks[["mrs3m_class_24h"]]
  mrs3m_models <- artifact$model_tasks[["mrs3m_class_24h"]]
  expected_mrs3m_models <- c(
    "logistic", "lasso", "ridge", "elastic_net", "decision_tree",
    "cart", "random_forest", "xgboost", "neural_network"
  )
  expected_mrs3m_kinds <- c(
    logistic = "logistic_classification",
    lasso = "logistic_classification",
    ridge = "logistic_classification",
    elastic_net = "logistic_classification",
    decision_tree = "ctree_classification",
    cart = "cart_classification",
    random_forest = "random_forest_classification",
    xgboost = "xgboost_classification",
    neural_network = "neural_network_classification"
  )
  expected_mrs3m_inputs <- c(
    "age_years_analysis", "sesso_m_0_f_1", "m_rs_pre_evento_0_5",
    "nihss_allingresso_numeric", "nihss_24h_numeric"
  )
  if (is.null(mrs3m_contract) || is.null(mrs3m_task) || is.null(mrs3m_models)) {
    errors <- c(errors, "task mRS a 3 mesi o relativo contratto assente")
  } else {
    if (!identical(mrs3m_contract$task_id, "mrs3m_class_24h") ||
        !identical(mrs3m_contract$landmark, "24 hours") ||
        !identical(mrs3m_contract$cohort_n, 82L) ||
        !identical(mrs3m_contract$events, 22L) ||
        !identical(mrs3m_contract$default_model_id, "lasso") ||
        !identical(mrs3m_contract$candidate_models, 9L)) {
      errors <- c(errors, "contratto del task mRS a 3 mesi incompatibile")
    }
    if (!identical(mrs3m_task$model_id, "lasso") ||
        !identical(mrs3m_task$best_model_id, "lasso") ||
        !identical(mrs3m_task$landmark, "24 ore") ||
        !identical(mrs3m_task$raw_inputs, expected_mrs3m_inputs) ||
        length(mrs3m_models) != 9L ||
        !identical(sort(names(mrs3m_models)), sort(expected_mrs3m_models))) {
      errors <- c(errors, "implementazione del task mRS a 3 mesi incompatibile")
    }
    model_schema_ok <- vapply(expected_mrs3m_models, function(model_id) {
      model_task <- mrs3m_models[[model_id]]
      !is.null(model_task) &&
        identical(model_task$task_id, "mrs3m_class_24h") &&
        identical(model_task$model_id, model_id) &&
        identical(model_task$model_kind, unname(expected_mrs3m_kinds[[model_id]])) &&
        identical(model_task$outcome_type, "classification") &&
        identical(model_task$raw_inputs, expected_mrs3m_inputs) &&
        identical(model_task$selected_design_features, expected_mrs3m_inputs) &&
        identical(colnames(model_task$background_x), expected_mrs3m_inputs)
    }, logical(1))
    if (!all(model_schema_ok)) {
      errors <- c(errors, "schema dei modelli mRS a 3 mesi incompatibile")
    }
  }
  unique(errors)
}

rebuild_requested <- tolower(Sys.getenv("SU2026_REBUILD_ARTIFACT", "false")) %in% c("1", "true", "yes")
load_valid_artifact <- function() {
  if (file.exists(artifact_path)) {
    candidate <- readRDS(artifact_path)
    errors <- artifact_validation_errors(candidate)
    if (length(errors) == 0) return(candidate)
    if (!rebuild_requested) {
      stop(
        "Artefatto predittivo obsoleto o non sicuro: ", paste(errors, collapse = "; "),
        ". Impostare SU2026_REBUILD_ARTIFACT=1 per ricostruirlo dal run configurato."
      )
    }
  } else if (!rebuild_requested) {
    stop(
      "Artefatto predittivo non trovato: ", artifact_path,
      ". Impostare SU2026_REBUILD_ARTIFACT=1 per costruirlo dal run configurato."
    )
  }
  training_env <- new.env(parent = globalenv())
  sys.source(training_code_path, envir = training_env, chdir = TRUE)
  training_env$build_artifacts()
  candidate <- readRDS(artifact_path)
  errors <- artifact_validation_errors(candidate)
  if (length(errors) > 0) stop("Artefatto ricostruito ma non valido: ", paste(errors, collapse = "; "))
  candidate
}

artifacts <- load_valid_artifact()

txt <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- ""
  stringr::str_squish(y)
}
num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
sigmoid <- function(x) 1 / (1 + exp(-x))
input_id <- function(variable) paste0("var__", variable)
pretty_value <- function(x, digits = 3) ifelse(is.na(x), "NA", sprintf(paste0("%.", digits, "f"), x))

ivt_timing_vars <- c("door_to_needle_min_recalc", "onset_to_needle_min_recalc")
evt_timing_vars <- c(
  "onset_to_groin_min_recalc",
  "door_to_groin_min_recalc",
  "groin_to_tici_min_recalc",
  "onset_to_tici_min_recalc"
)

is_yes <- function(x) {
  y <- num(x)
  length(y) > 0 && !is.na(y[[1]]) && y[[1]] == 1
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

conditional_nonapplicable_vars <- function(task, values) {
  hidden <- character()
  if ("ivt_0_no_1_si" %in% task$raw_inputs && !is_yes(values[["ivt_0_no_1_si"]])) {
    hidden <- c(hidden, intersect(ivt_timing_vars, task$raw_inputs))
  }
  if ("evt_si_no" %in% task$raw_inputs && !is_yes(values[["evt_si_no"]])) {
    hidden <- c(hidden, intersect(c(evt_timing_vars, "m_tici_grade"), task$raw_inputs))
  }
  unique(hidden)
}

apply_conditional_derivations <- function(task, values) {
  if ("ivt_0_no_1_si" %in% task$raw_inputs && !is_yes(values[["ivt_0_no_1_si"]])) {
    for (v in intersect(ivt_timing_vars, task$prepared_predictors)) {
      values[[v]] <- NA_real_
    }
    for (v in intersect(ivt_timing_vars, task$raw_inputs)) {
      values[[v]] <- NA_real_
    }
  }
  if ("evt_si_no" %in% task$raw_inputs && !is_yes(values[["evt_si_no"]])) {
    for (v in intersect(evt_timing_vars, task$prepared_predictors)) {
      values[[v]] <- NA_real_
    }
    for (v in intersect(evt_timing_vars, task$raw_inputs)) {
      values[[v]] <- NA_real_
    }
    if ("m_tici_grade" %in% task$raw_inputs || "m_tici_grade" %in% task$prepared_predictors) {
      values[["m_tici_grade"]] <- "Non EVT / non applicabile"
    }
  }
  if ("m_tici_grade" %in% names(values) && "evt_si_no" %in% names(values)) {
    evt_num <- num(values[["evt_si_no"]])
    if (is.na(evt_num) || evt_num != 1) {
      values[["m_tici_grade"]] <- "Non EVT / non applicabile"
    }
  }
  if ("successful_recanalization" %in% task$prepared_predictors || "successful_recanalization" %in% task$raw_inputs) {
    values[["successful_recanalization"]] <- derive_successful_recanalization(
      values[["evt_si_no"]] %||% NA,
      values[["m_tici_grade"]] %||% "Non EVT / non applicabile"
    )
  }
  values
}

is_blank_value <- function(x) {
  is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || !nzchar(trimws(as.character(x)))))
}

validate_input_values <- function(task, values) {
  if (is.null(task) || is.null(task$raw_inputs)) stop("Task predittivo non valido.")
  if (is.null(values) || !is.list(values)) stop("I valori di input devono essere un oggetto nominato.")
  unknown <- setdiff(names(values), setdiff(task$raw_inputs, "successful_recanalization"))
  if (length(unknown) > 0) stop("Variabili di input non riconosciute: ", paste(unknown, collapse = ", "))

  validated <- values
  hidden <- conditional_nonapplicable_vars(task, validated)
  missing_imputed <- character()
  extrapolation_warnings <- character()
  for (nm in task$raw_inputs) {
    if (nm == "successful_recanalization" || nm %in% hidden) next
    info <- task$variable_info[[nm]]
    if (is.null(info)) stop("Schema input assente per: ", nm)
    value <- validated[[nm]]
    if (identical(info$type, "numeric")) {
      if (is_blank_value(value)) {
        if (!isTRUE(info$allow_missing)) stop("Valore richiesto mancante: ", info$label, " (", nm, ")")
        validated[[nm]] <- NA_real_
        missing_imputed <- c(missing_imputed, nm)
        next
      }
      if (length(value) != 1) stop("Valore non scalare per: ", nm)
      parsed <- num(value)
      if (length(parsed) != 1 || !is.finite(parsed)) stop("Valore numerico non valido per: ", info$label, " (", nm, ")")
      if (!is.null(info$min) && is.finite(info$min) && parsed < info$min) {
        stop(info$label, " deve essere >= ", info$min, ".")
      }
      if (!is.null(info$max) && is.finite(info$max) && parsed > info$max) {
        stop(info$label, " deve essere <= ", info$max, ".")
      }
      if (!is.null(info$allowed_values) && length(info$allowed_values) > 0 && !(parsed %in% info$allowed_values)) {
        stop(
          "Valore non ammesso per ", info$label, " (", nm, "). Valori consentiti: ",
          paste(info$allowed_values, collapse = ", "), "."
        )
      }
      if (isTRUE(info$warn_outside_training_range) &&
          !is.null(info$training_min) && is.finite(info$training_min) &&
          !is.null(info$training_max) && is.finite(info$training_max) &&
          (parsed < info$training_min || parsed > info$training_max)) {
        extrapolation_warnings <- c(
          extrapolation_warnings,
          paste0(
            info$label, " = ", parsed,
            " è fuori dall'intervallo osservato nel training [",
            info$training_min, ", ", info$training_max, "]"
          )
        )
      }
      validated[[nm]] <- parsed
    } else if (identical(info$type, "categorical")) {
      if (is_blank_value(value)) {
        stop(
          "Selezionare un valore per ", info$label, " (", nm, "). ",
          "Per un dato realmente mancante scegliere esplicitamente 'missing_or_not_applicable', se disponibile."
        )
      }
      parsed <- txt(value)
      if (length(parsed) != 1 || !(parsed %in% info$levels)) {
        stop("Livello non valido per ", info$label, " (", nm, "): ", paste(parsed, collapse = ", "))
      }
      validated[[nm]] <- parsed
    } else {
      stop("Tipo di input non supportato per: ", nm)
    }
  }

  validated <- apply_conditional_derivations(task, validated)
  warning_messages <- character()
  if (length(missing_imputed) > 0) {
    warning_messages <- c(
      warning_messages,
      paste0("Valori mancanti imputati secondo il preprocessing del modello: ", paste(missing_imputed, collapse = ", "))
    )
  }
  if (length(extrapolation_warnings) > 0) {
    warning_messages <- c(
      warning_messages,
      paste0(
        "Attenzione, la stima richiede estrapolazione: ",
        paste(unique(extrapolation_warnings), collapse = "; "), "."
      )
    )
  }
  list(
    values = validated,
    warnings = warning_messages,
    imputed_variables = unique(missing_imputed)
  )
}

predict_task <- function(task, x) {
  x <- as.matrix(x[, task$selected_design_features, drop = FALSE])
  if (task$model_kind == "lasso_classification") {
    prob <- as.numeric(stats::predict(task$model, newx = x, s = "lambda.1se", type = "response"))
    link <- as.numeric(stats::predict(task$model, newx = x, s = "lambda.1se", type = "link"))
    return(list(value = prob, link = link))
  }
  if (task$model_kind == "logistic_classification") {
    beta <- task$model$coefficients
    intercept <- beta[["(Intercept)"]] %||% 0
    slopes <- beta[task$safe_names]
    slopes[is.na(slopes)] <- 0
    link <- as.numeric(intercept + as.matrix(x) %*% slopes)
    prob <- sigmoid(link)
    prob <- pmin(pmax(prob, 1e-6), 1 - 1e-6)
    return(list(value = prob, link = link))
  }
  if (task$model_kind %in% c("ctree_classification", "cart_classification")) {
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- task$safe_names
    pr <- stats::predict(task$model, newdata = x_df, type = "prob")
    if (is.list(pr)) pr <- do.call(rbind, pr)
    prob <- as.numeric(pr[, "positive"])
    prob <- pmin(pmax(prob, 1e-6), 1 - 1e-6)
    return(list(value = prob, link = qlogis(prob)))
  }
  if (task$model_kind == "random_forest_classification") {
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- task$safe_names
    pr <- stats::predict(task$model, data = x_df)$predictions
    prob <- as.numeric(pr[, "positive"])
    prob <- pmin(pmax(prob, 1e-6), 1 - 1e-6)
    return(list(value = prob, link = qlogis(prob)))
  }
  if (task$model_kind == "xgboost_classification") {
    prob <- as.numeric(stats::predict(task$model, xgboost::xgb.DMatrix(data = as.matrix(x))))
    prob <- pmin(pmax(prob, 1e-6), 1 - 1e-6)
    return(list(value = prob, link = qlogis(prob)))
  }
  if (task$model_kind == "neural_network_classification") {
    x_scaled <- sweep(sweep(as.matrix(x), 2, task$model$center, "-"), 2, task$model$scale, "/")
    prob <- as.numeric(stats::predict(task$model$fit, newdata = x_scaled, type = "raw"))
    prob <- pmin(pmax(prob, 1e-6), 1 - 1e-6)
    return(list(value = prob, link = qlogis(prob)))
  }
  if (task$model_kind == "glmnet_regression") {
    value <- as.numeric(stats::predict(task$model, newx = x, s = "lambda.1se"))
    value <- pmin(pmax(value, task$target_bounds[[1]]), task$target_bounds[[2]])
    return(list(value = value, link = value))
  }
  if (task$model_kind == "linear_regression") {
    beta <- task$model$coefficients
    b0 <- beta[["(Intercept)"]] %||% 0
    co <- beta[colnames(x)]
    co[is.na(co)] <- 0
    value <- as.numeric(b0 + x %*% co)
    value <- pmin(pmax(value, task$target_bounds[[1]]), task$target_bounds[[2]])
    return(list(value = value, link = value))
  }
  if (task$model_kind == "random_forest_regression") {
    x_df <- as.data.frame(x, check.names = FALSE)
    names(x_df) <- task$safe_names
    value <- as.numeric(stats::predict(task$model, data = x_df)$predictions)
    value <- pmin(pmax(value, task$target_bounds[[1]]), task$target_bounds[[2]])
    return(list(value = value, link = value))
  }
  if (task$model_kind == "xgboost_regression") {
    value <- as.numeric(stats::predict(task$model, xgboost::xgb.DMatrix(data = as.matrix(x))))
    value <- pmin(pmax(value, task$target_bounds[[1]]), task$target_bounds[[2]])
    return(list(value = value, link = value))
  }
  if (task$model_kind == "neural_network_regression") {
    x_scaled <- sweep(sweep(as.matrix(x), 2, task$model$center, "-"), 2, task$model$scale, "/")
    value <- as.numeric(stats::predict(task$model$fit, newdata = x_scaled, type = "raw")) * task$model$y_scale + task$model$y_center
    value <- pmin(pmax(value, task$target_bounds[[1]]), task$target_bounds[[2]])
    return(list(value = value, link = value))
  }
  x_df <- as.data.frame(x, check.names = FALSE)
  names(x_df) <- task$safe_names
  value <- as.numeric(stats::predict(task$model, newdata = x_df))
  value <- pmin(pmax(value, task$target_bounds[[1]]), task$target_bounds[[2]])
  list(value = value, link = value)
}

make_design_row <- function(task, values) {
  values <- apply_conditional_derivations(task, values)
  row <- list()
  for (nm in task$prepared_predictors) {
    info <- task$variable_info[[nm]]
    if (stringr::str_detect(nm, "_missing_flag$")) {
      base <- stringr::str_replace(nm, "_missing_flag$", "")
      base_value <- values[[base]]
      miss <- is.null(base_value) || length(base_value) == 0 || is.na(base_value)
      flag_levels <- info$design_levels %||% info$levels %||% c("observed", "missing")
      row[[nm]] <- factor(ifelse(miss, "missing", "observed"), levels = flag_levels)
    } else if (!is.null(info) && info$type == "numeric") {
      value <- num(values[[nm]])
      if (length(value) == 0 || is.na(value)) value <- info$median
      row[[nm]] <- value
    } else if (!is.null(info) && info$type == "categorical") {
      value <- if (is.null(values[[nm]]) || length(values[[nm]]) == 0) "" else txt(values[[nm]])
      if (value == "" && !(nm %in% task$raw_inputs)) value <- info$default
      if (value == "") stop("Valore categoriale mancante non validato per: ", nm)
      if (!(value %in% info$levels)) stop("Livello categoriale non valido per: ", nm)
      row[[nm]] <- factor(value, levels = info$design_levels %||% info$levels)
    }
  }

  row_df <- as.data.frame(row, check.names = FALSE)
  mm <- stats::model.matrix(~ . - 1, data = row_df)
  missing_cols <- setdiff(task$design_columns, colnames(mm))
  if (length(missing_cols) > 0) {
    mm <- cbind(mm, matrix(0, nrow = nrow(mm), ncol = length(missing_cols), dimnames = list(NULL, missing_cols)))
  }
  mm <- mm[, task$design_columns, drop = FALSE]
  mm[, task$selected_design_features, drop = FALSE]
}

linear_shap <- function(task, x) {
  x <- as.numeric(x[1, task$selected_design_features, drop = TRUE])
  names(x) <- task$selected_design_features
  bg <- task$column_means[task$selected_design_features]
  bg[is.na(bg)] <- 0

  if (task$model_kind == "lasso_classification") {
    co <- as.matrix(stats::coef(task$model, s = "lambda.1se"))
    beta <- as.numeric(co[, 1])
    names(beta) <- rownames(co)
    intercept <- beta[["(Intercept)"]] %||% 0
    beta <- beta[task$selected_design_features]
    beta[is.na(beta)] <- 0
    contrib <- beta * (x - bg)
    base <- intercept + sum(beta * bg)
    pred <- base + sum(contrib)
    scale <- "log-odds"
  } else if (task$model_kind == "glmnet_regression") {
    co <- as.matrix(stats::coef(task$model, s = "lambda.1se"))
    beta <- as.numeric(co[, 1])
    names(beta) <- rownames(co)
    intercept <- beta[["(Intercept)"]] %||% 0
    beta <- beta[task$selected_design_features]
    beta[is.na(beta)] <- 0
    contrib <- beta * (x - bg)
    base <- intercept + sum(beta * bg)
    pred <- base + sum(contrib)
    scale <- "scala outcome"
  } else if (task$model_kind == "logistic_classification") {
    beta <- task$model$coefficients
    intercept <- beta[["(Intercept)"]] %||% 0
    beta <- beta[make.names(task$selected_design_features, unique = TRUE)]
    names(beta) <- task$selected_design_features
    beta[is.na(beta)] <- 0
    contrib <- beta * (x - bg)
    base <- intercept + sum(beta * bg)
    pred <- base + sum(contrib)
    scale <- "log-odds"
  } else {
    beta <- task$model$coefficients
    intercept <- beta[["(Intercept)"]] %||% 0
    beta <- beta[task$selected_design_features]
    beta[is.na(beta)] <- 0
    contrib <- beta * (x - bg)
    base <- intercept + sum(beta * bg)
    pred <- base + sum(contrib)
    scale <- "scala outcome"
  }
  data.frame(
    feature = names(contrib),
    value = as.numeric(x),
    contribution = as.numeric(contrib),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(dplyr::desc(abs(contribution))) |>
    dplyr::mutate(base_value = base, prediction_link = pred, shap_scale = scale)
}

tree_shap <- function(task, x, nsim = 80) {
  x <- as.matrix(x[, task$selected_design_features, drop = FALSE])
  bg <- as.matrix(task$background_x[, task$selected_design_features, drop = FALSE])
  features <- colnames(bg)
  p <- length(features)
  contrib <- setNames(rep(0, p), features)
  if (nrow(bg) == 0 || p == 0) {
    return(data.frame(feature = character(), value = numeric(), contribution = numeric()))
  }

  set.seed(as.integer((task$training_seed %||% 1701L) + 1701L))
  rows <- sample(seq_len(nrow(bg)), nsim, replace = TRUE)
  for (i in seq_len(nsim)) {
    current <- matrix(bg[rows[[i]], ], nrow = 1, dimnames = list(NULL, features))
    previous <- predict_task(task, current)$value[[1]]
    for (j in sample(features)) {
      current[, j] <- x[, j]
      new_pred <- predict_task(task, current)$value[[1]]
      contrib[[j]] <- contrib[[j]] + (new_pred - previous)
      previous <- new_pred
    }
  }
  contrib <- contrib / nsim
  data.frame(
    feature = names(contrib),
    value = as.numeric(x[1, names(contrib)]),
    contribution = as.numeric(contrib),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(dplyr::desc(abs(contribution))) |>
    dplyr::mutate(
      base_value = mean(predict_task(task, bg)$value, na.rm = TRUE),
      prediction_link = predict_task(task, x)$value[[1]],
      shap_scale = "scala outcome"
    )
}

explain_prediction <- function(task, x) {
  if (task$model_kind %in% c("lasso_classification", "glmnet_regression", "logistic_classification", "linear_regression")) {
    linear_shap(task, x)
  } else {
    tree_shap(task, x, nsim = 120)
  }
}

format_force_value <- function(x) {
  if (is.na(x)) return("")
  if (abs(x - round(x)) < 1e-8) return(as.character(round(x)))
  pretty_value(x, 2)
}

force_plot_payload <- function(shap, max_features = 9) {
  shap <- shap[is.finite(shap$contribution) & !is.na(shap$contribution), , drop = FALSE]
  base <- unique(shap$base_value)[[1]]
  pred <- unique(shap$prediction_link)[[1]]
  scale <- unique(shap$shap_scale)[[1]]

  shap <- shap[order(abs(shap$contribution), decreasing = TRUE), , drop = FALSE]
  if (nrow(shap) > max_features) {
    other <- shap[(max_features + 1):nrow(shap), , drop = FALSE]
    shap <- shap[seq_len(max_features), , drop = FALSE]
    shap <- rbind(
      shap,
      data.frame(
        feature = "Altre feature",
        value = NA_real_,
        contribution = sum(other$contribution, na.rm = TRUE),
        base_value = base,
        prediction_link = pred,
        shap_scale = scale,
        stringsAsFactors = FALSE
      )
    )
  }
  shap <- shap[abs(shap$contribution) > 1e-10, , drop = FALSE]

  neg <- shap[shap$contribution < 0, , drop = FALSE]
  pos <- shap[shap$contribution >= 0, , drop = FALSE]
  neg <- neg[order(neg$contribution), , drop = FALSE]
  pos <- pos[order(pos$contribution, decreasing = TRUE), , drop = FALSE]

  segments <- data.frame()
  current <- base
  if (nrow(neg) > 0) {
    for (i in seq_len(nrow(neg))) {
      next_x <- current + neg$contribution[[i]]
      segments <- rbind(
        segments,
        data.frame(
          segment_id = paste0("neg_", i),
          feature = neg$feature[[i]],
          value = neg$value[[i]],
          contribution = neg$contribution[[i]],
          x0 = next_x,
          x1 = current,
          direction = "Riduce predizione",
          stringsAsFactors = FALSE
        )
      )
      current <- next_x
    }
  }

  current <- base + sum(neg$contribution, na.rm = TRUE)
  if (nrow(pos) > 0) {
    for (i in seq_len(nrow(pos))) {
      next_x <- current + pos$contribution[[i]]
      segments <- rbind(
        segments,
        data.frame(
          segment_id = paste0("pos_", i),
          feature = pos$feature[[i]],
          value = pos$value[[i]],
          contribution = pos$contribution[[i]],
          x0 = current,
          x1 = next_x,
          direction = "Aumenta predizione",
          stringsAsFactors = FALSE
        )
      )
      current <- next_x
    }
  }

  axis_values <- c(base, pred, segments$x0, segments$x1)
  axis_range <- diff(range(axis_values, na.rm = TRUE))
  if (!is.finite(axis_range) || axis_range == 0) axis_range <- 1
  x_min <- min(axis_values, na.rm = TRUE) - axis_range * 0.12
  x_max <- max(axis_values, na.rm = TRUE) + axis_range * 0.12

  polygon <- data.frame()
  if (nrow(segments) > 0) {
    for (i in seq_len(nrow(segments))) {
      left <- min(segments$x0[[i]], segments$x1[[i]])
      right <- max(segments$x0[[i]], segments$x1[[i]])
      width <- right - left
      head <- min(width * 0.42, axis_range * 0.028)
      head <- max(head, min(width * 0.25, axis_range * 0.01))
      y_low <- 0.40
      y_mid <- 0.52
      y_high <- 0.64
      if (segments$contribution[[i]] >= 0) {
        xs <- c(left, right - head, right, right - head, left)
        ys <- c(y_low, y_low, y_mid, y_high, y_high)
      } else {
        xs <- c(left, left + head, right, right, left + head)
        ys <- c(y_mid, y_low, y_low, y_high, y_high)
      }
      polygon <- rbind(
        polygon,
        data.frame(
          segment_id = segments$segment_id[[i]],
          x = xs,
          y = ys,
          direction = segments$direction[[i]],
          stringsAsFactors = FALSE
        )
      )
    }

    segments$mid <- (segments$x0 + segments$x1) / 2
    segments$width <- abs(segments$x1 - segments$x0)
    segments$contribution_label <- ifelse(
      segments$width >= axis_range * 0.04,
      sprintf("%+.3f", segments$contribution),
      ""
    )
    segments$text_color <- ifelse(segments$direction == "Aumenta predizione", "#171717", "#FFFFFF")
    segments$feature_label <- stringr::str_trunc(
      paste0(segments$feature, ifelse(is.na(segments$value), "", paste0("=", vapply(segments$value, format_force_value, character(1))))),
      width = 54
    )
    segments$label_y <- 0.84 + 0.12 * ((seq_len(nrow(segments)) + 1) %% 2)
  }

  list(
    segments = segments,
    polygon = polygon,
    base = base,
    pred = pred,
    scale = scale,
    x_min = x_min,
    x_max = x_max
  )
}

task_choices <- setNames(names(artifacts$tasks), vapply(artifacts$tasks, `[[`, character(1), "title"))

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      :root {
        --ink: #152033;
        --muted: #667085;
        --quiet: #8A94A6;
        --line: #DDE5EF;
        --line-strong: #C8D3E1;
        --panel: rgba(255, 255, 255, 0.94);
        --panel-soft: rgba(248, 251, 253, 0.92);
        --blue: #2557B8;
        --blue-dark: #173A78;
        --teal: #148A86;
        --gold: #D6A72D;
        --wine: #A12C5E;
        --rose: #C95B6A;
        --shadow: 0 18px 42px rgba(30, 41, 59, 0.11);
        --shadow-soft: 0 10px 24px rgba(30, 41, 59, 0.08);
      }
      html, body { min-height: 100%; }
      body {
        color: var(--ink);
        background:
          linear-gradient(90deg, rgba(37, 87, 184, 0.045) 1px, transparent 1px) 0 0 / 36px 36px,
          linear-gradient(180deg, rgba(20, 138, 134, 0.040) 1px, transparent 1px) 0 0 / 36px 36px,
          linear-gradient(135deg, #F8FAFC 0%, #EEF4F8 48%, #FBF8F2 100%);
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      .container-fluid {
        width: 100%;
        max-width: 1560px;
        padding-left: 22px;
        padding-right: 22px;
      }
      .app-header {
        position: relative;
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 18px;
        margin: 18px 0 16px 0;
        padding: 18px 20px;
        border: 1px solid rgba(255, 255, 255, 0.92);
        border-radius: 8px;
        background: rgba(255, 255, 255, 0.78);
        box-shadow: var(--shadow-soft);
        backdrop-filter: blur(18px);
        overflow: hidden;
        animation: surfaceIn 520ms cubic-bezier(.2, .8, .2, 1) both;
      }
      .app-header::before {
        content: '';
        position: absolute;
        left: 0;
        right: 0;
        top: 0;
        height: 4px;
        background: linear-gradient(90deg, var(--blue), var(--teal), var(--gold), var(--wine));
        background-size: 200% 100%;
        animation: sweep 10s ease-in-out infinite alternate;
      }
      .brand-lockup {
        display: flex;
        align-items: center;
        gap: 14px;
        min-width: 0;
      }
      .brand-mark {
        width: 42px;
        height: 42px;
        display: grid;
        place-items: center;
        flex: 0 0 auto;
        border-radius: 8px;
        color: #FFFFFF;
        background: linear-gradient(145deg, var(--blue), var(--teal));
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.35), 0 10px 20px rgba(37, 87, 184, 0.25);
      }
      .app-title {
        margin: 0;
        font-size: clamp(22px, 3vw, 30px);
        line-height: 1.08;
        letter-spacing: 0;
        font-weight: 760;
        white-space: normal;
      }
      .app-kicker {
        margin-top: 4px;
        color: var(--muted);
        font-size: 13px;
        line-height: 1.35;
      }
      .header-pills {
        display: flex;
        flex-wrap: wrap;
        justify-content: flex-end;
        gap: 8px;
      }
      .status-pill,
      .path-pill {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        min-height: 30px;
        padding: 6px 10px;
        border-radius: 8px;
        border: 1px solid rgba(200, 211, 225, 0.86);
        background: rgba(255, 255, 255, 0.76);
        color: #344054;
        font-size: 12px;
        font-weight: 680;
        white-space: nowrap;
      }
      .status-pill::before,
      .path-pill::before {
        content: '';
        width: 7px;
        height: 7px;
        border-radius: 50%;
        background: var(--teal);
      }
      .path-pill.off::before { background: var(--quiet); }
      .path-pill.on::before { background: var(--blue); }
      .path-pill.warn::before { background: var(--gold); }
      .subtle {
        color: var(--muted);
        font-size: 13px;
        line-height: 1.45;
      }
      .app-grid {
        display: grid;
        grid-template-columns: minmax(292px, 352px) minmax(0, 1fr);
        gap: 18px;
        align-items: start;
      }
      .main-stack {
        display: grid;
        gap: 16px;
        min-width: 0;
      }
      .control-panel,
      .summary-panel,
      .force-panel,
      .input-panel,
      .table-panel {
        border: 1px solid rgba(223, 229, 238, 0.92);
        border-radius: 8px;
        background: var(--panel);
        box-shadow: var(--shadow);
        backdrop-filter: blur(18px);
        animation: surfaceIn 560ms cubic-bezier(.2, .8, .2, 1) both;
      }
      .control-panel {
        position: sticky;
        top: 14px;
        padding: 16px;
      }
      .summary-panel { padding: 16px; animation-delay: 60ms; }
      .force-panel { padding: 16px 18px 2px 18px; animation-delay: 100ms; }
      .input-panel { padding: 16px; animation-delay: 130ms; }
      .table-panel { padding: 16px; min-width: 0; animation-delay: 160ms; }
      .table-grid {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 0.85fr);
        gap: 16px;
      }
      .panel-heading-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        margin-bottom: 12px;
      }
      .panel-eyebrow {
        margin: 0 0 4px 0;
        color: var(--blue);
        font-size: 11px;
        line-height: 1.2;
        text-transform: uppercase;
        letter-spacing: .08em;
        font-weight: 760;
      }
      .section-title {
        margin: 0;
        font-size: 16px;
        font-weight: 760;
        letter-spacing: 0;
      }
      .control-panel .form-group {
        margin-bottom: 14px;
      }
      .control-label {
        margin-bottom: 6px;
        color: #344054;
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: .075em;
        font-weight: 760;
      }
      .selectize-input,
      .form-control {
        min-height: 42px;
        border-radius: 8px !important;
        border-color: #D5DEE9 !important;
        background: rgba(255, 255, 255, 0.94) !important;
        box-shadow: none !important;
        transition: border-color 160ms ease, box-shadow 160ms ease, transform 160ms ease;
      }
      .selectize-input.focus,
      .form-control:focus {
        border-color: rgba(37, 87, 184, 0.55) !important;
        box-shadow: 0 0 0 4px rgba(37, 87, 184, 0.10) !important;
      }
      .btn-primary {
        width: 100%;
        min-height: 44px;
        margin-top: 4px;
        border: 0;
        border-radius: 8px;
        padding: 11px 14px;
        font-weight: 760;
        letter-spacing: 0;
        background: linear-gradient(135deg, var(--blue), var(--teal));
        box-shadow: 0 12px 24px rgba(37, 87, 184, 0.24);
        transition: transform 160ms ease, box-shadow 160ms ease, filter 160ms ease;
      }
      .btn-primary:hover,
      .btn-primary:focus {
        transform: translateY(-1px);
        filter: saturate(1.05);
        box-shadow: 0 16px 30px rgba(37, 87, 184, 0.30);
      }
      .model-note {
        margin-top: 14px;
        padding-top: 14px;
        border-top: 1px solid var(--line);
      }
      .note-grid {
        display: grid;
        gap: 10px;
      }
      .note-row {
        display: grid;
        grid-template-columns: 88px minmax(0, 1fr);
        gap: 10px;
        align-items: baseline;
      }
      .note-key {
        color: var(--quiet);
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: .07em;
        font-weight: 760;
      }
      .note-value {
        min-width: 0;
        color: #273447;
        font-size: 13px;
        line-height: 1.35;
        overflow-wrap: anywhere;
      }
      .metric-grid {
        display: grid;
        grid-template-columns: minmax(230px, 1.1fr) minmax(190px, .75fr) minmax(220px, .95fr);
        gap: 14px;
      }
      .metric-card {
        position: relative;
        min-height: 126px;
        border: 1px solid rgba(200, 211, 225, 0.92);
        border-radius: 8px;
        padding: 16px;
        background: linear-gradient(145deg, rgba(255,255,255,0.98), rgba(245,248,252,0.96));
        box-shadow: inset 0 1px 0 rgba(255,255,255,0.95);
        overflow: hidden;
        transform: translateZ(0);
      }
      .metric-card::after {
        content: '';
        position: absolute;
        left: 14px;
        right: 14px;
        bottom: 0;
        height: 3px;
        background: linear-gradient(90deg, var(--blue), var(--teal), var(--gold));
      }
      .metric-primary {
        border-color: rgba(37, 87, 184, 0.22);
        background: linear-gradient(145deg, #FFFFFF, #F2F7FF);
      }
      .metric-label {
        color: var(--muted);
        font-size: 12px;
        font-weight: 720;
        text-transform: uppercase;
        letter-spacing: .07em;
      }
      .prediction-value {
        margin: 9px 0 8px 0;
        font-size: clamp(34px, 5vw, 58px);
        font-weight: 820;
        line-height: 1;
        color: var(--blue-dark);
        font-variant-numeric: tabular-nums;
      }
      .metric-value-small {
        margin-top: 10px;
        color: #1D2939;
        font-size: clamp(18px, 2.2vw, 25px);
        line-height: 1.16;
        font-weight: 760;
        overflow-wrap: anywhere;
      }
      .metric-foot {
        color: var(--muted);
        font-size: 12.5px;
        line-height: 1.35;
        overflow-wrap: anywhere;
      }
      .probability-track {
        height: 8px;
        margin-top: 12px;
        border-radius: 999px;
        background: #E7EEF6;
        overflow: hidden;
      }
      .probability-fill {
        height: 100%;
        border-radius: 999px;
        background: linear-gradient(90deg, var(--teal), var(--blue), var(--wine));
        transition: width 500ms cubic-bezier(.2, .8, .2, 1);
      }
      .pathway-strip {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
        margin-top: 12px;
      }
      .input-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(240px, 1fr));
        gap: 10px 12px;
      }
      .input-cell {
        min-height: 86px;
        border: 1px solid rgba(223, 229, 238, 0.95);
        border-radius: 8px;
        padding: 10px 11px 0 11px;
        background: rgba(255, 255, 255, 0.86);
        transition: transform 180ms ease, box-shadow 180ms ease, border-color 180ms ease;
      }
      .input-cell:hover {
        transform: translateY(-2px);
        border-color: rgba(37, 87, 184, 0.35);
        box-shadow: 0 12px 28px rgba(21, 32, 53, 0.10);
      }
      .notice-cell {
        display: flex;
        flex-direction: column;
        justify-content: center;
        border-style: dashed;
        background: rgba(255, 250, 235, 0.72);
      }
      .dataTables_wrapper {
        font-size: 13px;
      }
      table.dataTable thead th {
        border-bottom: 1px solid var(--line-strong) !important;
        color: #344054;
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: .06em;
      }
      table.dataTable tbody td {
        border-top: 1px solid #EEF2F6 !important;
      }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        border: 1px solid #D5DEE9;
        border-radius: 8px;
        padding: 5px 8px;
        background: rgba(255,255,255,0.94);
      }
      .animate-on-update {
        will-change: transform, box-shadow;
      }
      .pulse-update {
        animation: updatePulse 620ms cubic-bezier(.2, .8, .2, 1);
      }
      .research-warning {
        margin: 0 0 16px 0;
        padding: 12px 16px;
        border: 1px solid #D6A72D;
        border-radius: 8px;
        background: #FFF8E1;
        color: #6B4E00;
        font-weight: 700;
      }
      @keyframes surfaceIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      @keyframes sweep {
        from { background-position: 0% 50%; }
        to { background-position: 100% 50%; }
      }
      @keyframes updatePulse {
        0% { transform: translateY(0); box-shadow: var(--shadow); }
        38% { transform: translateY(-3px); box-shadow: 0 22px 52px rgba(30, 41, 59, 0.16); }
        100% { transform: translateY(0); box-shadow: var(--shadow); }
      }
      @media (max-width: 1050px) {
        .app-grid { grid-template-columns: 1fr; }
        .control-panel { position: static; }
        .metric-grid,
        .table-grid { grid-template-columns: 1fr; }
        .app-header { align-items: flex-start; flex-direction: column; }
        .header-pills { justify-content: flex-start; }
      }
      @media (max-width: 760px) {
        .container-fluid { padding-left: 12px; padding-right: 12px; }
        .input-grid { grid-template-columns: 1fr; }
        .brand-mark { width: 38px; height: 38px; }
      }
    ")),
    tags$script(HTML("
      $(document).on('shiny:value', function(event) {
        if (event.name === 'prediction_card' || event.name === 'shap_plot') {
          setTimeout(function() {
            $('.animate-on-update').removeClass('pulse-update');
            void document.body.offsetWidth;
            $('.animate-on-update').addClass('pulse-update');
          }, 0);
        }
      });
    "))
  ),
  div(
    class = "app-header",
    div(
      class = "brand-lockup",
      div(class = "brand-mark", tags$span(class = "glyphicon glyphicon-stats", `aria-hidden` = "true")),
      div(
        div(class = "app-title", "SU 2026 - Stroke Prediction Studio"),
        div(class = "app-kicker", "Strumento sperimentale di ricerca con spiegazione SHAP locale.")
      )
    ),
    div(
      class = "header-pills",
      span(class = "status-pill", "Nested 5-fold CV"),
      span(class = "status-pill", "Feature set validato"),
      span(class = "status-pill", "SHAP locale"),
      span(class = "status-pill", "Research only")
    )
  ),
  div(
    class = "research-warning",
    "RESEARCH USE ONLY — NOT FOR CLINICAL USE. Le stime non sono validate per diagnosi, triage o decisioni terapeutiche individuali."
  ),
  div(
    class = "app-grid",
    div(
      class = "control-panel",
      div(class = "panel-eyebrow", "Setup"),
      div(class = "section-title", "Predizione"),
      selectInput("task_id", "Task predittivo", choices = task_choices, selected = "mrs3m_class_24h"),
      uiOutput("model_selector"),
      selectInput("record_id", "Modalità input", choices = "Manuale", selected = "Manuale"),
      actionButton("predict_btn", "Predici", icon = icon("flash", lib = "glyphicon"), class = "btn-primary"),
      div(class = "model-note", htmlOutput("model_notes"))
    ),
    div(
      class = "main-stack",
      uiOutput("prediction_card"),
      div(
        class = "force-panel animate-on-update",
        div(
          class = "panel-heading-row",
          div(
            div(class = "panel-eyebrow", "Explainable AI"),
            div(class = "section-title", "SHAP force plot")
          ),
          span(class = "status-pill", "Input-specifico")
        ),
        plotOutput("shap_plot", height = 520),
        div(
          class = "subtle",
          "I contributi SHAP descrivono associazioni condizionali del modello, non effetti protettivi o causali."
        )
      ),
      div(
        class = "input-panel",
        div(
          class = "panel-heading-row",
          div(
            div(class = "panel-eyebrow", "Input"),
            div(class = "section-title", "Variabili paziente")
          )
        ),
        uiOutput("dynamic_inputs")
      ),
      div(
        class = "table-grid",
        div(
          class = "table-panel",
          div(class = "panel-eyebrow", "SHAP"),
          div(class = "section-title", "Contributi locali"),
          DTOutput("shap_table")
        ),
        div(
          class = "table-panel",
          div(class = "panel-eyebrow", "Modello"),
          div(class = "section-title", "Feature selezionate"),
          DTOutput("feature_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  base_task <- reactive({
    task <- artifacts$tasks[[input$task_id]]
    validate(need(!is.null(task), paste0("Task inesistente: ", input$task_id)))
    task
  })

  output$model_selector <- renderUI({
    task <- base_task()
    model_set <- artifacts$model_tasks[[input$task_id]] %||% stats::setNames(list(task), task$model_id)
    choices <- stats::setNames(
      names(model_set),
      vapply(model_set, function(model_task) {
        badges <- as.character(model_task$model_badges %||% character())
        suffix <- if (length(badges) > 0) {
          paste0(" (", paste(badges, collapse = "; "), ")")
        } else if (isTRUE(model_task$is_best_model)) {
          " (migliore)"
        } else {
          ""
        }
        paste0(model_task$model_label, suffix)
      }, character(1))
    )
    selected <- task$best_model_id %||% task$model_id %||% names(model_set)[[1]]
    selectInput("model_id", "Modello predittivo", choices = choices, selected = selected)
  })

  current_task <- reactive({
    task <- base_task()
    model_set <- artifacts$model_tasks[[input$task_id]] %||% list()
    selected <- input$model_id
    if (is.null(selected) || !selected %in% names(model_set)) {
      selected <- task$best_model_id %||% task$model_id
    }
    model_set[[selected]] %||% task
  })

  observeEvent(input$task_id, {
    current_task()
    updateSelectInput(session, "record_id", choices = "Manuale", selected = "Manuale")
  }, ignoreInit = FALSE)

  selected_record <- reactive({
    task <- current_task()
    if (is.null(input$record_id) || input$record_id == "Manuale") return(NULL)
    stop("Record non disponibile: l'artefatto non contiene dati paziente.")
  })

  output$model_notes <- renderUI({
    task <- current_task()
    rows <- Filter(Negate(is.null), list(
      if (length(task$model_badges %||% character()) > 0) {
        div(class = "note-row", span(class = "note-key", "Ruolo"), span(class = "note-value", paste(task$model_badges, collapse = "; ")))
      },
      div(class = "note-row", span(class = "note-key", "Modello"), span(class = "note-value", task$model_label)),
      div(class = "note-row", span(class = "note-key", "Scenario"), span(class = "note-value", task$scenario)),
      if (!is.null(task$landmark)) {
        div(class = "note-row", span(class = "note-key", "Landmark"), span(class = "note-value", task$landmark))
      },
      if (!is.null(task$selection_criterion)) {
        div(class = "note-row", span(class = "note-key", "Selezione"), span(class = "note-value", task$selection_criterion))
      },
      div(class = "note-row", span(class = "note-key", "CV"), span(class = "note-value", task$performance)),
      div(class = "note-row", span(class = "note-key", "Deployment"), span(class = "note-value", "Refit sperimentale non validato indipendentemente")),
      div(class = "note-row", span(class = "note-key", "Feature"), span(class = "note-value", length(task$selected_design_features))),
      if (!is.null(task$limitations)) {
        div(class = "note-row", span(class = "note-key", "Limiti"), span(class = "note-value", task$limitations))
      }
    ))
    div(class = "note-grid", tagList(rows))
  })

  output$dynamic_inputs <- renderUI({
    task <- current_task()
    rec <- selected_record()
    current_values <- list()
    for (v in task$raw_inputs) {
      info <- task$variable_info[[v]]
      if (is.null(info)) next
      current <- input[[input_id(v)]]
      if (!is.null(current)) {
        current_values[[v]] <- current
      } else {
        current_values[[v]] <- ""
      }
    }
    hidden_vars <- conditional_nonapplicable_vars(task, current_values)
    controls <- lapply(task$raw_inputs, function(v) {
      if (v == "successful_recanalization") return(NULL)
      if (v %in% hidden_vars) return(NULL)
      info <- task$variable_info[[v]]
      if (is.null(info)) return(NULL)
      rec_value <- ""
      if (!is.null(current_values[[v]])) rec_value <- current_values[[v]]
      control <- if (info$type == "numeric" && !is.null(info$allowed_values) && length(info$allowed_values) > 0) {
        selected <- txt(rec_value)
        allowed <- as.character(info$allowed_values)
        if (!(selected %in% allowed)) selected <- ""
        selectInput(input_id(v), info$label, choices = c("-- Seleziona --" = "", allowed), selected = selected)
      } else if (info$type == "numeric") {
        bounds <- c(info$min, info$max)
        bounds <- bounds[is.finite(bounds)]
        range_hint <- if (length(bounds) == 2) paste0("Intervallo ammesso: ", bounds[[1]], "–", bounds[[2]]) else "Inserire un valore numerico"
        textInput(input_id(v), info$label, value = txt(rec_value), placeholder = range_hint)
      } else {
        value <- txt(rec_value)
        if (!(value %in% info$levels)) value <- ""
        selectInput(input_id(v), info$label, choices = c("-- Seleziona --" = "", info$levels), selected = value)
      }
      training_range_note <- if (isTRUE(info$warn_outside_training_range) &&
          !is.null(info$training_min) && is.finite(info$training_min) &&
          !is.null(info$training_max) && is.finite(info$training_max)) {
        paste0("Intervallo osservato nel training: ", info$training_min, "–", info$training_max)
      } else {
        NULL
      }
      tags$div(
        class = "input-cell",
        control,
        if (!is.null(training_range_note)) tags$div(class = "subtle", training_range_note)
      )
    })
    tags$div(
      class = "input-grid",
      controls,
      if (length(hidden_vars) > 0) {
        tags$div(
          class = "input-cell notice-cell",
          tags$div(class = "panel-eyebrow", "Timing non applicabili"),
          tags$div(class = "subtle", paste(hidden_vars, collapse = ", "))
        )
      }
    )
  })

  collect_values <- reactive({
    task <- current_task()
    values <- list()
    for (v in task$raw_inputs) {
      if (v == "successful_recanalization") next
      values[[v]] <- input[[input_id(v)]]
    }
    apply_conditional_derivations(task, values)
  })

  prediction_result <- eventReactive(input$predict_btn, {
    task <- current_task()
    checked <- tryCatch(
      validate_input_values(task, collect_values()),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      }
    )
    req(checked)
    if (length(checked$warnings) > 0) {
      showNotification(paste(checked$warnings, collapse = " "), type = "warning", duration = 10)
    }
    x <- make_design_row(task, checked$values)
    pred <- predict_task(task, x)
    shap <- explain_prediction(task, x)
    list(task = task, x = x, prediction = pred, shap = shap, warnings = checked$warnings)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  output$prediction_card <- renderUI({
    res <- prediction_result()
    req(res)
    task <- res$task
    value <- res$prediction$value[[1]]
    values <- collect_values()
    has_pathway <- any(c("ivt_0_no_1_si", "evt_si_no") %in% task$raw_inputs)
    ivt_active <- "ivt_0_no_1_si" %in% names(values) && is_yes(values[["ivt_0_no_1_si"]])
    evt_active <- "evt_si_no" %in% names(values) && is_yes(values[["evt_si_no"]])
    ivt_class <- if (ivt_active) "path-pill on" else "path-pill off"
    evt_class <- if (evt_active) "path-pill on" else "path-pill off"
    timing_class <- if (ivt_active || evt_active) "path-pill warn" else "path-pill off"
    if (task$outcome_type == "classification") {
      main <- sprintf("%.1f%%", 100 * value)
      label <- paste0("Probabilità di ", task$positive_label)
      fill_width <- sprintf("%.1f%%", 100 * value)
    } else {
      main <- pretty_value(value, 2)
      label <- paste("Predizione", task$units)
      bounds <- task$target_bounds %||% c(0, max(value, 1))
      low <- bounds[[1]]
      high <- bounds[[2]]
      fill_width <- sprintf("%.1f%%", 100 * pmin(pmax((value - low) / max(high - low, 1e-6), 0), 1))
    }
    div(
      class = "summary-panel animate-on-update",
      div(
        class = "metric-grid",
        div(
          class = "metric-card metric-primary",
          div(class = "metric-label", label),
          div(class = "prediction-value", main),
          div(class = "probability-track", div(class = "probability-fill", style = paste0("width:", fill_width, ";"))),
          if (has_pathway) {
            div(class = "pathway-strip", span(class = ivt_class, "IVT"), span(class = evt_class, "EVT"), span(class = timing_class, "Timing"))
          }
        ),
        div(
          class = "metric-card",
          div(class = "metric-label", "Modello"),
          div(class = "metric-value-small", task$model_label),
          div(class = "metric-foot", task$title)
        ),
        div(
          class = "metric-card",
          div(class = "metric-label", "Validazione"),
          div(class = "metric-value-small", length(task$selected_design_features)),
          div(class = "metric-foot", paste("Feature selezionate.", task$performance))
        )
      )
    )
  })

  output$shap_plot <- renderPlot({
    res <- prediction_result()
    req(res)
    task <- res$task
    fp <- force_plot_payload(res$shap, max_features = 9)
    if (nrow(fp$segments) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0, label = "Nessun contributo SHAP diverso da zero", size = 5) +
          theme_void()
      )
    }

    ggplot() +
      geom_vline(xintercept = fp$base, color = "#3B4352", linetype = "22", linewidth = 0.55) +
      geom_vline(xintercept = fp$pred, color = "#141922", linewidth = 0.65) +
      geom_polygon(
        data = fp$polygon,
        aes(x = x, y = y, group = segment_id, fill = direction),
        color = "#111827",
        linewidth = 0.35
      ) +
      geom_segment(
        data = fp$segments,
        aes(x = mid, xend = mid, y = 0.66, yend = label_y - 0.035),
        color = "#C7CDD8",
        linewidth = 0.45
      ) +
      geom_text(
        data = fp$segments,
        aes(x = mid, y = label_y, label = feature_label),
        color = "#222B3A",
        size = 3.35,
        lineheight = 0.95,
        check_overlap = TRUE
      ) +
      geom_text(
        data = fp$segments,
        aes(x = mid, y = 0.52, label = contribution_label, color = text_color),
        size = 4.4,
        fontface = "bold"
      ) +
      annotate(
        "text",
        x = fp$base,
        y = 0.20,
        label = paste0("E[f(x)]=", pretty_value(fp$base, 3)),
        color = "#1F2937",
        size = 3.7,
        vjust = 1,
        hjust = ifelse(fp$base > fp$pred, 0, 1)
      ) +
      annotate(
        "text",
        x = fp$pred,
        y = 0.20,
        label = paste0("f(x)=", pretty_value(fp$pred, 3)),
        color = "#1F2937",
        size = 3.7,
        vjust = 1,
        hjust = ifelse(fp$pred > fp$base, 0, 1)
      ) +
      scale_fill_manual(values = c("Aumenta predizione" = "#F2C94C", "Riduce predizione" = "#9E2F61")) +
      scale_color_identity() +
      coord_cartesian(xlim = c(fp$x_min, fp$x_max), ylim = c(0.10, 1.08), clip = "off") +
      labs(
        title = "SHAP force plot",
        subtitle = paste(task$model_label, "-", fp$scale),
        x = paste0("Prediction (", fp$scale, ")"),
        y = NULL,
        fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 21, face = "bold", color = "#111827", margin = margin(b = 2)),
        plot.subtitle = element_text(size = 12.5, color = "#5B6472", margin = margin(b = 14)),
        plot.margin = margin(22, 64, 32, 28),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 12)),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key.width = grid::unit(18, "pt")
      )
  })

  output$shap_table <- renderDT({
    res <- prediction_result()
    req(res)
    res$shap %>%
      mutate(
        value = round(value, 4),
        contribution = round(contribution, 4),
        base_value = round(base_value, 4),
        prediction_link = round(prediction_link, 4)
      ) %>%
      datatable(rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  })

  output$feature_table <- renderDT({
    task <- current_task()
    data.frame(
      selected_design_feature = task$selected_design_features,
      stringsAsFactors = FALSE
    ) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)
