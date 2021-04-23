plans <- list()

source("R/plan_example.R")

logger::log_info(glue::glue("bind plans: {paste(sort(names(plans)), collapse = ', ')}"))
plan <-
  plans %>%
  dplyr::bind_rows(.id = "df_name") %>%
  dplyr::arrange(df_name)
