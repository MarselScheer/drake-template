#-# bayes optimization

bayes_opt_svmrbf <- function(folds, bounds, init_grid_dt = init_grid_dt, n_iter) {
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
    verbose = TRUE)
  
  ret$History <- dplyr::rename(ret$History, AUC = Value)
  ret
}

#-# tune 

metric_profile_per_fold <- function(method, fold, tune_grid, ...) {
  
  fit <- function(tg_row) caret::train(y ~., method = method, data = fold$analysis, tuneGrid = tg_row, trControl = trainControl(method = "none"), ...)
  probs <- function(model) caret::predict.train(model, newdata = fold$assessment, type = 'prob')$PS
  
  if (is.null(tune_grid)) {
    return(
      data.frame(
        AUC = MLmetrics::AUC(
          y_pred = probs(fit(NULL)), 
          y_true = fold$assessment$y == 'PS'),
        grid = FALSE)
    )
  }
  
  ret <- tune_grid
  ret$AUC <- NA
  for (row in 1:nrow(tune_grid)) {
    ret$AUC[row] <- MLmetrics::AUC(
      y_pred = probs(fit(tune_grid[row, , drop = FALSE])), 
      y_true = fold$assessment$y == 'PS')
  }
  ret$grid <- TRUE
  ret
}


#-# recipe and folds

apply_recipe_to_folds <- function(rcp, folds){
  purrr::map(
    folds,
    function(f){
      rcp <- recipes::prep(rcp, training = f$analysis, retain = TRUE)
      list(analysis = recipes::juice(rcp), assessment = recipes::bake(rcp, new_data = f$assessment))
    }
  )
}


#-# wrangle 

wrangle <- function(df){
  df <- df %>% 
    dplyr::rename(y = class) %>% 
    dplyr::mutate(.set = tolower(.set)) %>% 
    h.y_as_first_col()
  
  
   df %>% 
    split(.$.set) %>% 
    lapply(function(d) dplyr::select(d, -.set))
}


#-# data import

get_segmentation_data <- function() {
  data("segmentationData", package = "caret")
  return(segmentationData)
}

load_fname <- function(fname, .set){
  flog.info(glue::glue("load {fname} and label it as {.set}"))
  suppressMessages(
    readr::read_csv(fname) %>%
      mutate(.set = .set) %>% 
      lowercase_names
  )
}

lowercase_names <- function(df){
  lc <- tolower(names(df))
  if (anyDuplicated(lc) > 0) {
    stop("lowercase varnames makes names ambiguous")
  }
  names(df) <- lc
  df
}


#-# helper-functions

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

h.add_list_name_as_column <- function(list_of_df){
  lapply(names(list_of_df), function(N){
    list_of_df[[N]]$df_name <- N
    list_of_df[[N]]
  }) %>% 
    dplyr::bind_rows()
}

h.bind_rows_with_id <- function(...) dplyr::bind_rows(..., .id = "target")

h.pj <- function(rcp) recipes::juice(recipes::prep(rcp, retain = TRUE))
h.ad <- as.data.frame

h.ls <- function(pattern = "") {
  grep(pattern, drake::cached(), value = TRUE)
}
