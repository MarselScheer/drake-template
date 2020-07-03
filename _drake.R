source("R/plan.R")
source("R/libs.R")
source("R/funs.R")

filter_selected_targets <- function(plan) {
  load(".settings.Rdata")
  ret <- c(TARGETS)
  if (!is.null(TARGETS_REGEXP)) {
    ret <- c(ret,
      grep(TARGETS_REGEXP, plan$target, value = TRUE))
  }

  if (is.null(ret)) {
    logger::log_info("Targets to be build: ALL")
    } else {
    logger::log_info(sprintf(
      "Targets to be build: %s",
      paste(ret, collapse = ", ")))
    }
  return(ret)
}


options(error = function() {
  if (!interactive()) {
    h_send_pushbullet(geterrmessage())
    dump.frames(to.file = TRUE)
    quit(status = 1)
  }
})


if (!dir.exists("reports")) {
  dir.create("reports", mode = "0755")
}

logger::log_info(glue::glue("bind plans: {paste(sort(names(plans)), collapse = ', ')}"))
plan <-
  plans %>%
  dplyr::bind_rows(.id = "df_name") %>%
  dplyr::arrange(df_name)

config <- drake::drake_config(
  plan = plan,
  targets = filter_selected_targets(plan = plan),
  cache_log_file = "cache_log.csv",
  parallelism = "future",
  jobs = 1
)
