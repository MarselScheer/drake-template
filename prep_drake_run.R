if (!("restrict_targets" %in% ls())) {
  source("R/infrastructure.R")
}
set_num_cpus(cpus = 1L)

restrict_targets(
#  targets = "my_data",
#  targets_regexp = "^m",
  dummy = "line allows to comment both target-arguments"
)

logger::log_info("Create drake-graph")
print(suppress_all(drake::r_vis_drake_graph()))

logger::log_info("Outdated objects:")
print(suppress_all(drake::r_outdated()))

logger::log_info("Predicted runtimes")
print(suppress_all((drake::r_predict_runtime(jobs = 1L))))

logger::log_info("Call execute_plans(cpus = <integer>) to start drake processing the plans.")
