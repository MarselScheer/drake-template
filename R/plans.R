plans <- list()

plans$p01_import <- 
  drake_plan(
    r = lowercase_names(dplyr::rename(get_segmentation_data(), .set = Case))
  ) 
flog.info("import defined")


plans$p02_wrangle <- 
  drake_plan(
    d = wrangle(r),
    rmarkdown::render(knitr_in("d_cleaning.Rmd"), output_file = file_out("./reports/d_cleaning.html"), quiet = TRUE)
  ) 
flog.info("wrangle defined")

