logger::log_info("Clean Global environment")
rm(list = ls(all.names = TRUE))
gc()

source("R/libs.R")
source("R/plan.R")
source("R/funs.R")

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

suppress_all <- function(...) {
  invisible(capture.output(el <- list(...)))
  return(el[[1]])
}

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

execute_plans <- function(NCPUS = 1) {
  logger::log_info("Starting drake")
  drake::r_make()
  logger::log_info("drake done.")
  h_send_pushbullet(
    glue::glue("drake done. plans: {paste0(unique(sub_plans$df_name), collapse = ', ')}"))
}
