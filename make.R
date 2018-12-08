rm(list = ls())

VFOLDS <- 2

source("R/libs.R")
source("R/funs.R")
source("R/plans.R")

if (!dir.exists("reports")) {
  dir.create("reports", mode = "0755")  
}

sub_plans <- plans#[1:3]
flog.info(glue::glue("bind plans: {paste(sort(names(sub_plans)), collapse = ', ')}"))
plan <- h.add_list_name_as_column(sub_plans) %>% 
  dplyr::arrange(df_name)

sub_plan <- plan 

config <- drake_config(sub_plan, cache_log_file = "cache_log.txt")
vis_drake_graph(config) %>% print


flog.info("run plan")
make(sub_plan, cache_log_file = "cache_log.txt")#, jobs = 7, seed = 123456)

#readd(d)$train %>% 
#  dplyr::as_tibble() %>% 
#  print()
