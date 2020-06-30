source("R/plan.R")
source("R/libs.R")
source("R/funs.R")

options(error = function() {
  if (!interactive()) {
    h_send_pushbullet(geterrmessage())
    dump.frames(to.file = TRUE)
    quit(status = 1)
  }
})


if (!dir.exists("reports")) {
  dir.create("reports", mode = "0755")
}

sub_plans <- plans #[1:13]
logger::log_info(glue::glue("bind plans: {paste(sort(names(sub_plans)), collapse = ', ')}"))
plan <-
  sub_plans %>%
  dplyr::bind_rows(.id = "df_name") %>%
  dplyr::arrange(df_name)


config <- drake::drake_config(
  plan = plan,
  targets = NULL,
  cache_log_file = "cache_log.csv",
  parallelism = "future",
  jobs = 1
)
