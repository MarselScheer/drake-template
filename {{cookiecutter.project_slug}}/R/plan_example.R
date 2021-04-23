plans <- list()

plans[["gen_data"]] <- drake::drake_plan(
  my_data = data.frame(
    a = rnorm(n = 100),
    b = rnorm(n = 100)
  )
)

plans[["model"]] <- drake::drake_plan(
  my_model = lm(formula = b ~ a, data = my_data),
  report =  drake::target(
    command = {
      rmarkdown::render(
        input = drake::knitr_in("reports/presentation_template.Rmd"),
        quiet = TRUE)
      # file_out is necessary to rebuild the target if the html
      # was modified/deleted
      drake::file_out("reports/presentation_template.html")
    })

)
