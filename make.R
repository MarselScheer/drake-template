rm(list = ls())

VFOLDS <- 2
MAX_EXPAND <- NULL

source("R/libs.R")
pkgconfig::set_config("drake::strings_in_dots" = "literals")

source("R/funs.R")
source("R/plans.R")

options(error = function() {
  if (!interactive()) {
    h.send_pushbullet(geterrmessage())
    dump.frames(to.file = TRUE)
    quit(status = 1)
  }
})


if (!dir.exists("reports")) {
  dir.create("reports", mode = "0755")
}

sub_plans <- plans #[1:13]
flog.info(glue::glue("bind plans: {paste(sort(names(sub_plans)), collapse = ', ')}"))
plan <-
  sub_plans %>%
  dplyr::bind_rows(.id = "df_name") %>%
  dplyr::arrange(df_name)

TARGETS <- NULL
#TARGETS <- c("plot_glm_profiles", "plot_rf_profiles")
#TARGETS <- grep("m_final", plan$target, value = TRUE)

config <- drake_config(
  plan, 
  targets = TARGETS,
  cache_log_file = "cache_log.txt")
vis_drake_graph(config)
vis_drake_graph(config, targets_only = TRUE) %>% print()


sub_plan <-
  plan %>%
  dplyr::filter(target %in% h.necessary_targets(config, TARGETS)) %>% 
  #h.plan_to_source() %>% 
  identity()





flog.info("start plan")
# TODO: the call "DT::datatable(options = list(pageLength = 100))" in rmd_cleaning_DT works only with lock_envir = FALSE ?!?
#make(sub_plan, cache_log_file = "cache_log.txt", lock_envir = TRUE) # , jobs = 7, seed = 123456)    
make(config = config)  


h.send_pushbullet(glue::glue("end TARGETS: {paste0(TARGETS, collapse = ', ')}"))
h.send_pushbullet(glue::glue("end plan: {paste0(unique(sub_plan$df_name), collapse = ', ')}"))
flog.info("end plan.")
# readd(d)$train %>%
#  dplyr::as_tibble() %>%
#  print()
