plans <- list()

plans[["gen_data"]] <- drake::drake_plan(
  my_data = data.frame(
    a = rnorm(n = 100),
    b = rnorm(n = 100)
  )
)

plans[["model"]] <- drake::drake_plan(
  my_model = lm(formula = b ~ a, data = my_data),
  report =  rmarkdown::render(
    input = drake::knitr_in("presentation_template.Rmd"),
    output_file = drake::file_out("reports/template.html"),
    quiet = TRUE)
)

logger::log_info(glue::glue("bind plans: {paste(sort(names(plans)), collapse = ', ')}"))
plan <-
  plans %>%
  dplyr::bind_rows(.id = "df_name") %>%
  dplyr::arrange(df_name)
