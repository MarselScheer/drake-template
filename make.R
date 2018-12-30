rm(list = ls())

VFOLDS <- 2

source("R/libs.R")
pkgconfig::set_config("drake::strings_in_dots" = "literals")

source("R/funs.R")
source("R/plans.R")

if (!dir.exists("reports")) {
  dir.create("reports", mode = "0755")
}

sub_plans <- plans # [1:3]
flog.info(glue::glue("bind plans: {paste(sort(names(sub_plans)), collapse = ', ')}"))
plan <-
  sub_plans %>%
  dplyr::bind_rows(.id = "df_name") %>%
  dplyr::arrange(df_name)

sub_plan <-
  plan %>%
  h.minimal_plan(grep("m_final", plan$target, value = TRUE)) %>%
  identity()

h.plan_to_source(sub_plan)

config <- drake_config(sub_plan, cache_log_file = "cache_log.txt")
vis_drake_graph(config, targets_only = TRUE) %>% print()

flog.info("start plan")
make(sub_plan, cache_log_file = "cache_log.txt") # , jobs = 7, seed = 123456)
flog.info("end plan.")
# readd(d)$train %>%
#  dplyr::as_tibble() %>%
#  print()
