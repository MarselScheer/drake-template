source("R/infrastructure.R")

config <- drake::drake_config(
  plan = plan,
  targets = filter_selected_targets(plan = plan),
  cache_log_file = "cache_log.csv",
  parallelism = "future",
  jobs = 1
)
