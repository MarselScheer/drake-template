source("R/infrastructure.R")


# NOTE (infrastructure): restrict the execution to a few targets here
TARGETS <- NULL
#TARGETS <- "my_data"
TARGETS_REGEXP <- NULL
#TARGETS_REGEXP <- "data$"

logger::log_info(paste("Save various settings that are loaded",
  "by _drake.R when execute_plans() invokes r_make()"))
save(TARGETS, TARGETS_REGEXP, file = ".settings.Rdata")

logger::log_info("Create drake-config")
source("_drake.R")

logger::log_info("Create drake-graph")
suppress_all(drake::r_vis_drake_graph())
logger::log_info("Outdated objects:")
suppress_all(drake::r_outdated())
logger::log_info("Predicted runtimes")
suppress_all((drake::r_predict_runtime(jobs = 1)))
logger::log_info("Call execute_plans() to start drake processing the plans.")
