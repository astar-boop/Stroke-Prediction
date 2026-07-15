args <- commandArgs(trailingOnly = TRUE)
action <- if (length(args) > 0) args[[1]] else "metadata"
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

input_text <- paste(readLines("stdin", warn = FALSE), collapse = "\n")
payload <- if (nzchar(input_text)) jsonlite::fromJSON(input_text, simplifyVector = FALSE) else list()

bridge_dir <- Sys.getenv("SU2026_WEB_DIR", "")
if (!nzchar(bridge_dir)) {
  file_arg <- commandArgs(FALSE)
  file_arg <- sub("^--file=", "", file_arg[grepl("^--file=", file_arg)][[1]])
  bridge_dir <- dirname(normalizePath(file_arg))
}
bridge_dir <- normalizePath(bridge_dir)
root_dir <- normalizePath(file.path(bridge_dir, ".."))
app_file <- file.path(root_dir, "SU2026_prediction_app", "app.R")

app_env <- new.env(parent = globalenv())
old_wd <- getwd()
setwd(dirname(app_file))
on.exit(setwd(old_wd), add = TRUE)
suppressPackageStartupMessages(suppressMessages(sys.source(app_file, envir = app_env)))
setwd(old_wd)

null_if_na <- function(x) {
  if (length(x) == 0 || is.null(x)) return(NULL)
  if (length(x) == 1 && (is.na(x) || is.nan(x) || is.infinite(x))) return(NULL)
  x
}

as_scalar <- function(x) {
  if (length(x) == 0 || is.null(x)) return(NULL)
  y <- x[[1]]
  if (is.factor(y)) y <- as.character(y)
  null_if_na(y)
}

task_by_id <- function(task_id, model_id = NULL) {
  if (is.null(task_id) || !is.character(task_id) || length(task_id) != 1 || !nzchar(task_id)) stop("task_id richiesto e deve essere una stringa non vuota.")
  if (is.null(app_env$artifacts$tasks[[task_id]])) stop("Task non trovato: ", task_id)
  model_sets <- app_env$artifacts$model_tasks %||% list()
  if (!is.null(model_id)) {
    if (!is.character(model_id) || length(model_id) != 1 || !nzchar(model_id)) {
      stop("model_id, se fornito, deve essere una stringa non vuota.")
    }
    task <- model_sets[[task_id]][[model_id]]
    if (is.null(task)) stop("Modello non trovato per il task ", task_id, ": ", model_id)
    return(task)
  }
  app_env$artifacts$tasks[[task_id]]
}

assert_manual_record <- function(record_id = NULL) {
  if (!is.null(record_id) && (!is.character(record_id) || length(record_id) != 1 || !nzchar(record_id))) {
    stop("record_id, se fornito, deve essere una stringa non vuota.")
  }
  if (!is.null(record_id) && record_id != "Manuale") {
    stop("Record non disponibile: l'artefatto non contiene dati paziente (record_id=", record_id, ").")
  }
  invisible(TRUE)
}

input_schema <- function(task) {
  out <- list()
  for (v in task$raw_inputs) {
    if (v == "successful_recanalization") next
    info <- task$variable_info[[v]]
    if (is.null(info)) next
    item <- list(
      variable = v,
      label = info$label,
      type = info$type
    )
    if (identical(info$type, "numeric")) {
      item$min <- null_if_na(info$min)
      item$max <- null_if_na(info$max)
      item$allowed_values <- null_if_na(info$allowed_values)
      item$allow_missing <- isTRUE(info$allow_missing)
      item$imputation_value <- null_if_na(info$median)
      item$training_min <- null_if_na(info$training_min)
      item$training_max <- null_if_na(info$training_max)
      item$warn_outside_training_range <- isTRUE(info$warn_outside_training_range)
    } else {
      item$levels <- as.character(info$levels)
      item$allow_missing <- isTRUE(info$allow_missing)
    }
    out[[length(out) + 1]] <- item
  }
  out
}

explanation_metadata <- function(task) {
  exact_kinds <- c("lasso_classification", "glmnet_regression", "logistic_classification", "linear_regression")
  approximate <- !(task$model_kind %in% exact_kinds)
  list(
    approximate = approximate,
    method = if (approximate) "Permutation-SHAP Monte Carlo" else "Decomposizione lineare esatta",
    note = if (approximate) {
      "Stima approssimata su permutazioni Monte Carlo; piccoli scarti additivi rispetto all'output sono attesi."
    } else {
      "Decomposizione additiva esatta sulla scala del modello."
    }
  )
}

metadata <- function() {
  tasks <- list()
  model_sets <- app_env$artifacts$model_tasks %||% list()
  for (id in names(app_env$artifacts$tasks)) {
    task <- app_env$artifacts$tasks[[id]]
    model_set <- model_sets[[id]] %||% stats::setNames(list(task), task$model_id %||% "default")
    model_options <- list()
    for (model_key in names(model_set)) {
      model_task <- model_set[[model_key]]
      is_best <- isTRUE(model_task$is_best_model) || identical(model_task$model_id, task$best_model_id)
      badges <- as.character(model_task$model_badges %||% character())
      option_suffix <- if (length(badges) > 0) {
        paste0(" (", paste(badges, collapse = "; "), ")")
      } else if (is_best) {
        " (migliore)"
      } else {
        ""
      }
      model_options[[length(model_options) + 1]] <- list(
        model_id = model_task$model_id %||% model_key,
        model_label = model_task$model_label,
        option_label = paste0(model_task$model_label, option_suffix),
        is_best = is_best,
        model_badges = badges,
        performance = model_task$performance,
        metrics = model_task$metrics %||% data.frame(),
        outcome_type = model_task$outcome_type,
        positive_label = model_task$positive_label %||% NULL,
        units = model_task$units %||% NULL,
        deployment_validation = model_task$deployment_validation %||% "unknown",
        landmark = model_task$landmark %||% NULL,
        selection_criterion = model_task$selection_criterion %||% NULL,
        limitations = model_task$limitations %||% NULL,
        cohort_summary = model_task$cohort_summary %||% NULL,
        explanation = explanation_metadata(model_task),
        selected_feature_count = length(model_task$selected_design_features),
        inputs = input_schema(model_task),
        record_ids = character()
      )
    }
    tasks[[length(tasks) + 1]] <- list(
      id = id,
      title = task$title,
      model_label = task$model_label,
      model_id = task$model_id %||% NULL,
      best_model_id = task$best_model_id %||% NULL,
      best_model_label = task$best_model_label %||% NULL,
      is_best_model = isTRUE(task$is_best_model),
      model_badges = as.character(task$model_badges %||% character()),
      scenario = task$scenario,
      performance = task$performance,
      metrics = task$metrics %||% data.frame(),
      outcome_type = task$outcome_type,
      positive_label = task$positive_label %||% NULL,
      units = task$units %||% NULL,
      deployment_validation = task$deployment_validation %||% "unknown",
      landmark = task$landmark %||% NULL,
      selection_criterion = task$selection_criterion %||% NULL,
      limitations = task$limitations %||% NULL,
      cohort_summary = task$cohort_summary %||% NULL,
      explanation = explanation_metadata(task),
      selected_feature_count = length(task$selected_design_features),
      inputs = input_schema(task),
      record_ids = character(),
      model_options = model_options
    )
  }
  list(
    generated_at = as.character(app_env$artifacts$generated_at),
    source_workbook = app_env$artifacts$source_workbook,
    source_run = app_env$artifacts$source_run,
    dataset_md5 = app_env$artifacts$manifest$dataset_md5,
    research_only = TRUE,
    contains_patient_records = FALSE,
    notes = as.character(app_env$artifacts$notes),
    tasks = tasks
  )
}

record_values <- function(payload) {
  task <- task_by_id(payload$task_id, payload$model_id %||% NULL)
  assert_manual_record(payload$record_id %||% NULL)
  list(
    values = list(),
    hidden_vars = character(),
    message = "Modalita manuale: nessun record paziente e serializzato nell'artefatto."
  )
}

prediction <- function(payload) {
  task <- task_by_id(payload$task_id, payload$model_id %||% NULL)
  assert_manual_record(payload$record_id %||% NULL)
  checked <- app_env$validate_input_values(task, payload$values %||% list())
  values <- checked$values
  x <- app_env$make_design_row(task, values)
  pred <- app_env$predict_task(task, x)
  shap <- app_env$explain_prediction(task, x)
  shap <- shap[is.finite(shap$contribution) & !is.na(shap$contribution), , drop = FALSE]
  imputed_variables <- as.character(checked$imputed_variables %||% character())
  imputation <- lapply(imputed_variables, function(variable) {
    list(
      variable = variable,
      value = null_if_na(task$variable_info[[variable]]$median)
    )
  })

  list(
    task = list(
      id = payload$task_id,
      title = task$title,
      model_id = task$model_id %||% NULL,
      model_label = task$model_label,
      best_model_id = task$best_model_id %||% NULL,
      best_model_label = task$best_model_label %||% NULL,
      is_best_model = isTRUE(task$is_best_model),
      model_badges = as.character(task$model_badges %||% character()),
      scenario = task$scenario,
      performance = task$performance,
      metrics = task$metrics %||% data.frame(),
      outcome_type = task$outcome_type,
      positive_label = task$positive_label %||% NULL,
      units = task$units %||% NULL,
      deployment_validation = task$deployment_validation %||% "unknown",
      landmark = task$landmark %||% NULL,
      selection_criterion = task$selection_criterion %||% NULL,
      limitations = task$limitations %||% NULL,
      cohort_summary = task$cohort_summary %||% NULL,
      explanation = explanation_metadata(task),
      selected_feature_count = length(task$selected_design_features),
      selected_design_features = as.character(task$selected_design_features)
    ),
    prediction = list(
      value = as.numeric(pred$value[[1]]),
      link = as.numeric(pred$link[[1]])
    ),
    values = values,
    warnings = checked$warnings,
    imputation = imputation,
    hidden_vars = app_env$conditional_nonapplicable_vars(task, values),
    shap = shap,
    feature_table = data.frame(
      selected_design_feature = as.character(task$selected_design_features),
      stringsAsFactors = FALSE
    )
  )
}

shap_summary <- function(payload) {
  task <- task_by_id(payload$task_id, payload$model_id %||% NULL)
  max_features <- as.integer(payload$max_features %||% 10)
  if (is.na(max_features) || max_features < 1L || max_features > 30L) stop("max_features deve essere compreso tra 1 e 30.")
  is_linear <- task$model_kind %in% c("lasso_classification", "glmnet_regression", "logistic_classification", "linear_regression")
  default_records <- if (is_linear) 80L else 24L
  max_records <- as.integer(payload$max_records %||% default_records)
  nsim <- as.integer(payload$nsim %||% if (is_linear) 0L else 24L)
  if (is.na(max_records) || max_records < 1L || max_records > 100L) stop("max_records deve essere compreso tra 1 e 100.")
  if (is.na(nsim) || nsim < 0L || nsim > 200L) stop("nsim deve essere compreso tra 0 e 200.")

  bg <- as.matrix(task$background_x[, task$selected_design_features, drop = FALSE])
  if (nrow(bg) == 0 || ncol(bg) == 0) {
    return(list(rows = data.frame(), summary = data.frame(), sampled_profiles = 0L))
  }
  n_sample <- min(max_records, nrow(bg))
  idx <- unique(round(seq(1, nrow(bg), length.out = n_sample)))

  parts <- vector("list", length(idx))
  for (i in seq_along(idx)) {
    x <- bg[idx[[i]], , drop = FALSE]
    sh <- if (is_linear) {
      app_env$linear_shap(task, x)
    } else {
      app_env$tree_shap(task, x, nsim = nsim)
    }
    sh$sample_index <- idx[[i]]
    parts[[i]] <- sh
  }
  dat <- do.call(rbind, parts)
  dat <- dat[is.finite(dat$contribution) & !is.na(dat$contribution), , drop = FALSE]
  if (nrow(dat) == 0) {
    return(list(rows = data.frame(), summary = data.frame(), sampled_profiles = length(idx)))
  }

  stats <- aggregate(abs(dat$contribution), list(feature = dat$feature), mean, na.rm = TRUE)
  names(stats)[names(stats) == "x"] <- "mean_abs_shap"
  stats <- stats[order(stats$mean_abs_shap, decreasing = TRUE), , drop = FALSE]
  top_features <- head(stats$feature, max_features)
  dat <- dat[dat$feature %in% top_features, , drop = FALSE]
  dat$feature <- factor(dat$feature, levels = top_features)
  dat <- dat[order(dat$feature, dat$sample_index), , drop = FALSE]

  stats <- stats[stats$feature %in% top_features, , drop = FALSE]
  stats$feature <- factor(stats$feature, levels = top_features)
  stats <- stats[order(stats$feature), , drop = FALSE]
  stats$n <- as.integer(vapply(as.character(stats$feature), function(f) sum(dat$feature == f), integer(1)))
  stats$min_shap <- vapply(as.character(stats$feature), function(f) min(dat$contribution[dat$feature == f], na.rm = TRUE), numeric(1))
  stats$max_shap <- vapply(as.character(stats$feature), function(f) max(dat$contribution[dat$feature == f], na.rm = TRUE), numeric(1))

  list(
    task_id = payload$task_id,
    model_label = task$model_label,
    shap_scale = unique(dat$shap_scale)[[1]] %||% "scala outcome",
    explanation = explanation_metadata(task),
    sampled_profiles = length(idx),
    rows = dat[, c("feature", "value", "contribution", "sample_index")],
    summary = stats
  )
}

result <- switch(
  action,
  metadata = metadata(),
  record = record_values(payload),
  predict = prediction(payload),
  shap_summary = shap_summary(payload),
  stop("Azione non supportata: ", action)
)

cat(jsonlite::toJSON(result, auto_unbox = TRUE, null = "null", na = "null", dataframe = "rows", digits = 8))
