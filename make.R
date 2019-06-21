rm(list = ls())


source("R/libs.R")
pkgconfig::set_config("drake::strings_in_dots" = "literals")

source("R/funs.R")
source("R/plans_3.R")

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
#TARGETS <- "bo_svm2"
#TARGETS <- c("a_rf1_10_200", "a_rf2_10_200")
#TARGETS <- grep("m_final", plan$target, value = TRUE)

config <- drake_config(
  plan, 
  targets = TARGETS, 
  cache_log_file = "cache_log.csv")
if (interactive()) {
  try(vis_drake_graph(config))
  try(vis_drake_graph(config, targets_only = TRUE) %>% print())
  drake::outdated(config) %>% print()
  cat("Start or type 'sc' to show changes of the cache?")
  ans <- readLines(n = 1)
  if (ans == "sc") {
    # NOTE: if a target is NOT created by a function that is imported, for instance by using target(purrr::map(.....)) directly
    #       in the plan, then updating the code directly in target(purrr::map(.....)) will not be visible in the comparison 
    #       of the cache-log because the hash of the target will change after make has rebuild the target. Therefore, targets 
    #       can be outdated AND it is not visible in the following cache-log-comparison.
    #       This is an argumet for creating dedicated functions for the targets.
    read_csv("cache_log.csv", col_types = cols()) %>%
      left_join(drake_cache_log(), by = "name") %>%
      filter(hash.x != hash.y) %>%
      select(name, hash.x, hash.y, -type.x, -type.y) %>% 
      print()
    ans <- readLines(n = 1)
  }
  if (ans != "") {
    stop("Aborted")
  }
}

# sub_plan <-
#   plan %>%
#   dplyr::filter(target %in% h.necessary_targets(config, TARGETS)) %>% 
#   #h.plan_to_source() %>% 
#   identity()





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
