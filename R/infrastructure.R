logger::log_info("Load infrastructure.R")
logger::log_info("Clean Global environment")
rm(list = ls(all.names = TRUE))
gc()

source("R/libs.R")
source("R/plan.R")
source("R/funs.R")

# send pushbullet in case of an error during
# non-interactive executation
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
##' Suppress logging messages for functions used directly by the user
##'
##' @param ... function-call like r_outdated()
##' @return output of the function-call
suppress_all <- function(...) {
  # HACK: for suppressing logging-messages while calleing
  #       r_vis_drake_graph, r_outdated, r_predict_runtime
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
##' @export
restrict_targets <- function(targets = NULL, targets_regexp = NULL, ...) {
  logger::log_info("Restrict targets which are build")

  TARGETS <- targets
  TARGETS_REGEXP <- targets_regexp
  logger::log_info(paste("Save various settings that are loaded",
    "by _drake.R when execute_plans() invokes r_make()"))
  save(TARGETS, TARGETS_REGEXP, file = ".settings.Rdata")

  source("_drake.R")
  return(invisible(NULL))
}
##' Filters certain targets from the defined drake-plan
##'
##' Targets defined in R/plans.R are filter according to
##' the definitions made with \link{restrict_targets}
##' @param plan a drake-plan (usually the plan defined by R/plans.R)
##' @return character vector with target-names or NULL which represents
##'   all targets
filter_selected_targets <- function(plan) {
  logger::log_info("Filter targets according to the defined restrictions")
  fname_settings <- ".settings.Rdata"
  if (!file.exists(fname_settings)) {
    logger::log_info("Targets to be build: ALL")
    return(NULL)
  }

  load(fname_settings)
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

##' Loads the number of cpus defined by \link{set_num_cpus}
##'
##' @return number defined by \link{set_num_cpus}
get_num_cpus <- function() {
  fname_cpu_settings <- ".ncpus.Rdata"
  if (!file.exists(fname_cpu_settings)) {
    logger::log_warn("Number of CPUs not defined. Use 1 CPU.")
    return(1)
  }

  load(fname_cpu_settings)
  ret <- NCPUS
  logger::log_info(sprintf("Numbers of CPUS to be used: %s", ret))
  future::plan(future::sequential)
  if (ret > 1) {
    future::plan(future::multiprocess)
  }
  return(ret)
}

##' Defines the number of cpus used by drake
##'
##' @param cpus integer. number of cpus used by drake
##' @return no explicit return value
##' @export
set_num_cpus <- function(cpus = 1L) {
  NCPUS <- cpus
  save(NCPUS, file = ".ncpus.Rdata")
  return(invisible(NULL))
}

##' Execute drake-plan from the console
##'
##' @param cpus integer. number of cpus used by drake
##' @return no explicit return value
##' @export
execute_plans <- function(cpus = 1L) {

  set_num_cpus(cpus = cpus)

  # make sure that current plan is loaded so that filtering
  # targets is performed correctly
  source("R/plan.R")
  cat("\n\nConfirm settings:\n\n")
  filter_selected_targets(plan = plan)
  get_num_cpus()
  if (interactive()) {
    answer <- readline(prompt = "Start drake? (y/N) ")
    if (answer != "y"){
      logger::log_error("Manually aborted")
      stop("Manually aborted")
    }
  }
  logger::log_info("Start drake-run")
  drake::r_make()
  logger::log_info("drake done")
  cat("\n\n") # want some space to set apart the log-msg of the next run
  h_send_pushbullet("drake done")

  # it is distracting to see output in the console after
  # the last logger-msg
  return(invisible(NULL))
}

logger::log_info("Loaded infrastructure.R")
