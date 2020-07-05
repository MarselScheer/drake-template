if (!("restrict_targets" %in% ls())) {
  source("R/infrastructure.R")
}

restrict_targets(
#  targets = "my_data",
#  targets_regexp = "^m",
  dummy = "line allows to comment both target-arguments"
)

logger::log_info("Create drake-graph")
suppress_all(drake::r_vis_drake_graph())

logger::log_info("Outdated objects:")
suppress_all(drake::r_outdated())

logger::log_info("Predicted runtimes")
suppress_all((drake::r_predict_runtime(jobs = 1)))

logger::log_info("Call execute_plans() to start drake processing the plans.")
