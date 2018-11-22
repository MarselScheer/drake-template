plans <- list()

plans$p01_import <- 
  drake_plan(
    r = lowercase_names(dplyr::rename(get_segmentation_data(), .set = Case))
  ) 
flog.info("import defined")


plans$p02_wrangle <- 
  drake_plan(
    d = wrangle(r),
    rmd_cleaning = rmarkdown::render(knitr_in("d_cleaning.Rmd"), output_file = file_out("./reports/d_cleaning.md"), quiet = TRUE),
    rmd_cleaning_DT = rmarkdown::render(knitr_in("d_cleaning_DT.Rmd"), output_file = file_out("./reports/d_cleaning_DT.html"), quiet = TRUE)
  ) 
flog.info("wrangle defined")

plans$p03_first_glance <- 
  drake_plan(
    d_cor = d$train %>% dplyr::select_if(is.numeric) %>% corrr::correlate(),
    rmd_first_glance = rmarkdown::render(knitr_in("d_first_glance.Rmd"), output_file = file_out("./reports/d_first_glance.html"), quiet = TRUE)
    #d_var_rank = funModeling::var_rank_info(d$train, "y") # takes quite a while
  )

