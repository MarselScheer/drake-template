logger::log_info("Load _drake.R")
source("R/infrastructure.R")

NCPUS <- get_num_cpus()
future::plan(future::sequential)
if (NCPUS > 1) {
  future::plan(future::multiprocess)
}

logger::log_info("Loaded _drake.R")
config <- drake::drake_config(
  plan = plan,
  targets = filter_selected_targets(plan = plan),
  cache_log_file = "cache_log.csv",
  parallelism = "future",
  jobs = NCPUS
)
