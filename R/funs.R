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
    h.log_end()
    return(
      data.frame(
        AUC = ModelMetrics::auc(
          predicted = probs(fit(NULL)),
          actual = fold$assessment$y == "PS"
        ),
        grid = FALSE
      )
    )
  }

  ret <- tune_grid
  ret$AUC <- NA
  for (row in 1:nrow(tune_grid)) {
    ret$AUC[row] <- ModelMetrics::auc(
      predicted = probs(fit(tune_grid[row, , drop = FALSE])),
      actual = fold$assessment$y == "PS"
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
  # in order to avoid self-invalidation in drake caused by modifying .GlobalEnv
  ne <- new.env()
  data("segmentationData", package = "caret", envir = ne)

  h.log_end()
  return(get("segmentationData", envir = ne))
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

h.send_pushbullet <- function(msg, title = NULL) {
  
  # pushbullet notifications can only be send
  # if ~/.rpushbullet.json exist
  # should follow
  #{
  #  "key": "..key.."
  #}
  #RPushbullet::pbSetup("..key..") can also be used
  #to generate the json-file.
  if (interactive()) {
    return()
  }
  
  if (is.null(title)) {
    title = glue::glue("{Sys.info()['nodename']}: {basename(getwd())}")
  }
  
  try(RPushbullet::pbPost(
    type = "note", 
    title = title, 
    body = glue::glue("{format(Sys.time(), '%Y-%m-%d %H:%M')}\n{msg}")
  ))
}

h.send_plot_pushbullet <- function(p, title = NULL){
  # pushbullet notifications can only be send
  # if ~/.rpushbullet.json exist
  # should follow
  #{
  #  "key": "..key.."
  #}
  #RPushbullet::pbSetup("..key..") can also be used
  #to generate the json-file.

  if (interactive()) {
    return()
  }
  
  if (is.null(title)) {
    title = glue::glue("{Sys.info()['nodename']}: {basename(getwd())}")
  }
  
  fName <- glue::glue("{tempfile()}.jpg")
  ggplot2::ggsave(fName, plot = p)
  try(RPushbullet::pbPost(
    type = "file",
    body = title, # title parameter does not work but body at the top and looks quite like title.
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


h.create_caption_txt <- function(desc, author, analysis_unique_id) {
  glue("{desc}
       {author}
       {analysis_unique_id}
       {date()}")
}

h.plot_info <- function(desc, author, analysis_unique_id) {
  ggplot2::labs(caption = h.create_caption_txt(desc, author, analysis_unique_id))
}

h.plot_info_font_size <- function(font.size) {
  ggplot2::theme(plot.caption = element_text(size = font.size))
}

h.html_to_human_readable <- function(s) {
  s <- gsub("&#124;", "  or  ", s, fixed = TRUE)
  s
}

h.annotate_with_df <- function(df, position = c("top-left", "top-right", "bottom-left", "bottom-right"), size = 3) {
  df_str <- knitr::kable(df) %>%
    as.character() %>%
    h.html_to_human_readable() %>%
    paste(collapse = "\n")
  
  position <- match.arg(position)
  
  if (position == "top-left") {
    return(ggplot2::annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, label = df_str, family = "mono", size = size))
  } else if (position == "top-right") {
    return(ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, label = df_str, family = "mono", size = size))
  } else if (position == "bottom-left") {
    return(ggplot2::annotate("text", x = -Inf, y = -Inf, hjust = 0, vjust = 0, label = df_str, family = "mono", size = size))
  } else if (position == "bottom-right") {
    return(ggplot2::annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = df_str, family = "mono", size = size))
  }
  stop("position unknown")
}


h.get_names <- function(...) {
  ret <- match.call()
  as.character(ret[-1])
}

h.bind_rows_with_id <- function(..., .id = "target") dplyr::bind_rows(..., .id = .id)

h.pj <- function(rcp) recipes::juice(recipes::prep(rcp, retain = TRUE))
h.ad <- as.data.frame

h.loadd_plan <- function(plan) {
  
  cat(glue::glue("Loading: {paste0(plan$target, collapse = ', ')}"))
  for (target in plan$target) {
    value <- do.call("readd", list(target = target))
    do.call("assign", list(x = target, value = value, envir = globalenv()))
  }
}

h.ls <- function(pattern = "") {
  grep(pattern, drake::cached(), value = TRUE)
}
