rm(list = ls())

VFOLDS <- 2

source("R/libs.R")
source("R/funs.R")
source("R/plans.R")


sub_plans <- plans#[1:3]
flog.info(glue::glue("bind plans: {paste(sort(names(sub_plans)), collapse = ', ')}"))
plan <- h.add_list_name_as_column(sub_plans) %>% 
  dplyr::arrange(df_name)

sub_plan <- plan 

config <- drake_config(sub_plan)
vis_drake_graph(config) %>% print



get_segmentation_data <- function() {
  data("segmentationData", package = "caret")
  return(segmentationData)
}
flog.info("run plan")
make(sub_plan, cache_log_file = "cache_log.txt")

#readd(d)$train %>% 
#  dplyr::as_tibble() %>% 
#  print()
