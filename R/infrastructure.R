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

##' Restrict targets used by drake
##'
##' @param targets vector of targets to be build by drake
##' @param targets_regexp regular expression that defines the
##'    targets to be build by drake
##' @param ... currently only used for convenience. This way one
##'    comment out the actual target-parameters without the need
##'    to take care of commas.
##' @return no explicit return-value
restrict_targets <- function(targets = NULL, targets_regexp = NULL, ...) {

  TARGETS <- targets
  TARGETS_REGEXP <- targets_regexp
  logger::log_info(paste("Save various settings that are loaded",
    "by _drake.R when execute_plans() invokes r_make()"))
  save(TARGETS, TARGETS_REGEXP, file = ".settings.Rdata")

  logger::log_info("Create drake-config")
  source("_drake.R")
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

get_num_cpus <- function() {
  load(".ncpus.Rdata")
  ret <- NCPUS
  logger::log_info(sprintf("Numbers of CPUS to be used: %s", ret))
  future::plan(future::sequential)
  if (ret > 1) {
    future::plan(future::multiprocess)
  }
  return(ret)
}

execute_plans <- function(NCPUS = 1, plan = plan) {
  save(NCPUS, file = ".ncpus.Rdata")
  cat("\n\nConfirm settings:\n\n")
  filter_selected_targets(plan = plan)
  get_num_cpus()
  answer <- readline(prompt = "Start drake? (y/N) ")
  if (answer != "y"){
    logger::log_error("Manually aborted")
    stop("Manually aborted")
  }
  logger::log_info("Starting drake")
  drake::r_make()
  logger::log_info("drake done.")
  cat("\n\n") # want some space to set apart the log-msg of the next run
  h_send_pushbullet(
    glue::glue("drake done. plans: {paste0(unique(sub_plans$df_name), collapse = ', ')}"))

  # it is distracting to see output in the console after
  # the last logger-msg
  return(invisible(NULL))
}
