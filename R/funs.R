#-# final model

final_model <- function(method, recipe, data, trControl = trainControl(method = "none"), ...) {
  h.log_start()

  ret <- caret::train(recipe, method = method, data = data, trControl = trControl, ...)

  h.log_end()
  ret
}

#-# bayes optimization

bayes_opt_svmrbf <- function(folds, bounds, init_grid_dt = init_grid_dt, n_iter) {
  h.log_start()

  FUN <- function(.sigma, .C) {
    txt <- capture.output(
      result <- purrr::map_dfr(folds, function(fold) metric_profile_per_fold("svmRadial", fold, tune_grid = data.frame(.sigma = .sigma, .C = .C), prob.model = TRUE))
    )
    list(Score = mean(result$AUC), Pred = 0)
  }

  init_grid_dt <-
    init_grid_dt %>%
    dplyr::group_by(.sigma, .C) %>%
    dplyr::summarise(Value = mean(AUC))

  ret <-
    rBayesianOptimization::BayesianOptimization(
      FUN,
      bounds = bounds,
      init_grid_dt = init_grid_dt,
      init_points = 0,
      n_iter = n_iter,
      acq = "ucb",
      verbose = TRUE
    )

  ret$History <- dplyr::rename(ret$History, AUC = Value)

  h.log_end()
  ret
}

#-# tune 

metric_profile_per_fold <- function(method, fold, tune_grid, ...) {
  h.log_start()

  fit <- function(tg_row) caret::train(y ~ ., method = method, data = fold$analysis, tuneGrid = tg_row, trControl = trainControl(method = "none"), ...)
  probs <- function(model) caret::predict.train(model, newdata = fold$assessment, type = "prob")$PS

  if (is.null(tune_grid)) {
    return(
      data.frame(
        AUC = MLmetrics::AUC(
          y_pred = probs(fit(NULL)),
          y_true = fold$assessment$y == "PS"
        ),
        grid = FALSE
      )
    )
  }

  ret <- tune_grid
  ret$AUC <- NA
  for (row in 1:nrow(tune_grid)) {
    ret$AUC[row] <- MLmetrics::AUC(
      y_pred = probs(fit(tune_grid[row, , drop = FALSE])),
      y_true = fold$assessment$y == "PS"
    )
  }
  ret$grid <- TRUE

  h.log_end()
  ret
}


#-# recipe and folds

apply_recipe_to_folds <- function(rcp, folds) {
  h.log_start()

  ret <- purrr::map(
    folds,
    function(f) {
      rcp <- recipes::prep(rcp, training = f$analysis, retain = TRUE)
      list(analysis = recipes::juice(rcp), assessment = recipes::bake(rcp, new_data = f$assessment))
    }
  )

  h.log_end()
  ret
}


#-# wrangle 

wrangle <- function(df) {
  h.log_start()

  df <- df %>%
    dplyr::rename(y = class) %>%
    dplyr::mutate(.set = tolower(.set)) %>%
    h.y_as_first_col()


  ret <-
    df %>%
    split(.$.set) %>%
    lapply(function(d) dplyr::select(d, -.set))

  h.log_end()
  ret
}


#-# data import

get_segmentation_data <- function() {
  h.log_start()

  data("segmentationData", package = "caret")

  h.log_end()
  return(segmentationData)
}

load_set <- function(fname, .set) {
  h.log_start()

  ret <- suppressMessages(
    readr::read_csv(fname) %>%
      mutate(.set = .set) %>%
      h.lowercase_names()
  )

  h.log_end()
  ret
}


#-# helper-functions

h.send_pushbullet <- function(msg, title = basename(getwd())) {
  
  # pushbullet notifications can only be send
  # if ~/.rpushbullet.json exist
  # should follow
  #{
  #  "key": "..key.."
  #}
  #RPushbullet::pbSetup("..key..") can also be used
  #to generate the json-file.
  
  
  if (!interactive()) {
    try(RPushbullet::pbPost(
      type = "note", 
      title = title, 
      body = glue::glue("{format(Sys.time(), '%Y-%m-%d %H:%M')}\n{msg}")
      )
    )
  }
}

h.send_plot_pushbullet <- function(p, title = basename(getwd())){
  # pushbullet notifications can only be send
  # if ~/.rpushbullet.json exist
  # should follow
  #{
  #  "key": "..key.."
  #}
  #RPushbullet::pbSetup("..key..") can also be used
  #to generate the json-file.
  
  
  fName <- glue::glue("{tempfile()}.jpg")
  ggsave(fName, plot = p)
  try(RPushbullet::pbPost(
    type = "file",
    title = title,
    body = "body",
    url = fName)
  )
}


h.lowercase_names <- function(df) {
  lc <- tolower(names(df))
  if (anyDuplicated(lc) > 0) {
    stop("lowercase varnames makes names ambiguous")
  }
  names(df) <- lc
  df
}


h.log_start <- function(send_pushbullet = FALSE) {
  mc <- sys.call(sys.parent())
  mc <- capture.output(print(mc))
  mc <- paste0(trimws(mc), collapse = " ")
  
  msg <- glue::glue("start {mc}")
  if (send_pushbullet) {
    h.send_pushbullet(msg)
  }
  flog.info(msg)
}
h.log_end <- function(send_pushbullet = FALSE) {
  mc <- sys.call(sys.parent())
  mc <- capture.output(print(mc))
  mc <- paste0(trimws(mc), collapse = " ")

  msg <- glue::glue("end {mc}")
  if (send_pushbullet) {
    h.send_pushbullet(msg)
  }
  flog.info(msg)
}

h.plan_to_source <- function(plan) {
  fName <- "plan_as_plain.R"
  drake::plan_to_code(plan, con = fName)

  code <- c(readLines("./R/libs.R"), readLines("./R/funs.R"), readLines(fName))

  writeLines(code, con = fName)
}


h.necessary_targets <- function(config, target) {
  drake_graph_info(config, from = target, mode = "in", targets_only = TRUE)$nodes$id
}

h.minimal_plan <- function(plan, targets) {
  config <- drake_config(plan)

  all_necessary_targets <- purrr::map(targets, purrr::partial(h.necessary_targets, config = config)) %>%
    unlist() %>%
    unique()

  plan %>%
    dplyr::filter(target %in% all_necessary_targets)
}

h.insert_base_plan <- function(plan, base_plan, base_plan_wildcard, rules = NULL, wildcard = NULL, values = NULL, expand = TRUE, rename = expand, trace = FALSE, columns = "command") {
  rules[[base_plan_wildcard]] <- base_plan[, "target", drop = TRUE]

  base_plan <- dplyr::select(base_plan, -dplyr::one_of("command"))
  by <- c("target")
  names(by) <- base_plan_wildcard

  evaluate_plan(plan, rules, wildcard, values, expand, rename, trace, columns) %>%
    dplyr::left_join(base_plan, by = by) %>%
    dplyr::select(-contains(base_plan_wildcard), -contains("__from"))
}

h.clear__ <- function(plan) {
  new_names <- gsub("__", "", names(plan))
  if (any(duplicated(new_names))) {
    stop("Duplicated names after removing __")
  }
  names(plan) <- new_names
  plan
}


h.y_as_first_col <- function(df) {
  df[, c("y", setdiff(names(df), "y"))]
}

h.bind_rows_with_id <- function(..., .id = "target") dplyr::bind_rows(..., .id = .id)

h.pj <- function(rcp) recipes::juice(recipes::prep(rcp, retain = TRUE))
h.ad <- as.data.frame

h.ls <- function(pattern = "") {
  grep(pattern, drake::cached(), value = TRUE)
}
