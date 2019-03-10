plans <- list()

plans$p00_general <-
  drake_plan(
    rmd_template = rmarkdown::render(knitr_in('presentation_template.Rmd'), output_file = file_out('./reports/00_template.html'), quiet = TRUE),
    AUTHOR = paste("Marsel Scheer"),
    ANALYSIS_UNIQUE_ID = paste("0e2278ab-9bc5-4018-90e9-3500fddc0926"),
    ABSTRACT = paste("Lorem ipsum dolor sit amet, consetetur sadipscing elitr")
  )


plans$p01_import <-
  drake_plan(
    r = h.lowercase_names(dplyr::rename(get_segmentation_data(), .set = Case))
  )


plans$p02_wrangle <-
  drake_plan(
    d = wrangle(r),
    rmd_cleaning = rmarkdown::render(knitr_in("d_cleaning.Rmd"), output_file = file_out("./reports/d_cleaning.md"), quiet = TRUE),
    rmd_cleaning_DT = rmarkdown::render(knitr_in("d_cleaning_DT.Rmd"), output_file = file_out("./reports/d_cleaning_DT.html"), quiet = TRUE)
  )

plans$p03_first_glance <-
  drake_plan(
    d_cor = d$train %>% dplyr::select_if(is.numeric) %>% corrr::correlate(),
    rmd_first_glance = rmarkdown::render(knitr_in("d_first_glance.Rmd"), output_file = file_out("./reports/d_first_glance.html"), quiet = TRUE)
    # d_var_rank = funModeling::var_rank_info(d$train, "y") # takes quite a while
  )

plans$p04_raw_folds <-
  drake_plan(
    raw_folds = purrr::map(
      rsample::vfold_cv(d$train, v = VFOLDS)$splits,
      function(s) list(analysis = rsample::analysis(s), assessment = rsample::assessment(s))
    )
  )

plans$p05_recipes_1_basic <-
  drake_plan(
    rcp_pure = recipes::recipe(y ~ ., data = head(d$train)) %>%
      step_rm(cell),
    rcp_basic = rcp_pure %>%
      step_YeoJohnson(all_predictors()) %>%
      step_center(all_predictors()) %>%
      step_scale(all_predictors())
  )


plans$p05_recipes_2_filter <- 
  drake_plan(
  rcp_filter = target(
    rcp_basic %>% filter(all_predictors(), threshold = threshold),
    transform = cross(filter = c(step_corr, step_pca), threshold = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99))
  ),
  trace = TRUE
) 



plans$p06_folds_1_pure <-
  drake_plan(
    fr_pure = apply_recipe_to_folds(rcp_pure, raw_folds)
  )


params <- 
  plans$p05_recipes_2_filter %>% 
  dplyr::select(rcp = target) %>% 
  dplyr::mutate_each(rlang::syms)

plans$p06_folds_1_filter <-
  drake_plan(
    fr = target(
      apply_recipe_to_folds(rcp, raw_folds),
      transform = map(.data = !!params)
    ),
    trace = TRUE
  ) 


params <- plans$p06_folds_1_pure %>% 
  dplyr::select(rcp = target) %>% 
  dplyr::mutate_each(rlang::syms)
  
  

plans$p07_model_tuning_1_rf <-
  drake_plan(
    m_rf = target(
      purrr::map_dfr(rcp, function(fold)
        metric_profile_per_fold(
          method = "rf",
          fold = fold,
          tune_grid = data.frame(mtry = c(5, 10, 20, 30, 40, 58))
        ),
        .id = "fold"
      ),
      transform = map(.data = !!params)
    ),
    profile_rf = target(
      purrr::map2_dfr(list(m_rf), h.get_names(rcp), function(x,y) {
        x$filter = y
        x
      }),
      transform = combine(m_rf, rcp)
    ),
    trace = TRUE
  )


params <- plans$p06_folds_1_filter %>% 
  dplyr::select(rcp = target) %>% 
  dplyr::mutate_each(rlang::syms)

plans$p07_model_tuning_1_glm <-
  drake_plan(
    m_glm = target(
      purrr::map_dfr(rcp, function(fold)
        metric_profile_per_fold(
          method = "glm",
          fold = fold,
          tune_grid = NULL
        ),
        .id = "fold"
      ),
      transform = map(.data = !!params)
    ),
    profile_glm = target(
      purrr::map2_dfr(list(m_glm), h.get_names(rcp), function(x,y) {
        x$filter = stringr::str_extract(y, "step_.*_")
        x$threshold = as.double(stringr::str_extract(y, "0.\\d{1,2}"))
        x
      }),
      transform = combine(m_glm, rcp)
    ),
    trace = TRUE
  ) 


params <- plans$p06_folds_1_filter %>% 
  dplyr::select(rcp = target) %>% 
  dplyr::mutate_each(rlang::syms)



plans$p07_model_tuning_1_svm <-
  drake_plan(
    m_svmRadial = target(
      purrr::map_dfr(rcp, function(fold)
        metric_profile_per_fold(
          method = "svmRadial",
          fold = fold,
          tune_grid = data.frame(.sigma = c(0.005, 0.01, 0.02), .C = 2^seq(-4, 4)),
          prob.model = TRUE
        ),
        .id = "fold"
      ),
      transform = map(.data = !!params)
    ),
    profile_svmRadial = target(
      purrr::map2_dfr(list(m_svmRadial), h.get_names(rcp), function(x,y) {
        x$filter = stringr::str_extract(y, "step_.*_")
        x$threshold = as.double(stringr::str_extract(y, "0.\\d{1,2}"))
        x
      }),
      transform = combine(m_svmRadial, rcp)
    ),
    trace = TRUE
  )


plans$p08_plots <-
  drake_plan(
    plot_rf_profiles = profile_rf %>% 
      dplyr::group_by(mtry) %>%
      dplyr::mutate(AUC_mean = mean(AUC), AUC_sd = sd(AUC)) %>%
      ggplot(aes(y = AUC, x = mtry)) +
      geom_line(aes(group = fold), alpha = 0.2) +
      geom_line(aes(y = AUC_mean)) +
      geom_errorbar(aes(ymin = AUC_mean - AUC_sd, ymax = AUC_mean + AUC_sd)),
    plot_svmrbf_profiles = profile_svmRadial %>%
      dplyr::group_by(filter, threshold, .sigma, .C) %>%
      dplyr::mutate(AUC_mean = mean(AUC), AUC_sd = sd(AUC)) %>%
      ggplot(aes(y = AUC, x = threshold, group = interaction(.C), color = interaction(.C))) +
      facet_grid(.sigma ~ filter, labeller = label_both) +
      geom_line(aes(y = AUC_mean)),
    plot_glm_profiles = profile_glm %>%
      dplyr::group_by(filter, threshold) %>%
      dplyr::mutate(AUC_mean = mean(AUC), AUC_sd = sd(AUC)) %>%
      ggplot(aes(y = AUC, x = threshold, color = filter)) +
      geom_line(aes(group = interaction(fold, filter), color = filter), alpha = 0.2) +
      geom_line(aes(y = AUC_mean, group = filter)) +
      geom_errorbar(aes(ymin = AUC_mean - AUC_sd, ymax = AUC_mean + AUC_sd, group = filter))
  )

plans$p09_bayes_opt <-
  drake::drake_plan(
    m_svmrbf_bayes_opt1 = bayes_opt_svmrbf(
      folds = fr_rcp_filter_step_pca_0.5,
      bounds = list(.sigma = c(0.00001, 0.1), .C = c(0.00001, 64)),
      init_grid_dt = m_svmRadial_fr_rcp_filter_step_pca_0.5,
      n_iter = 5
    ),
    m_svmrbf_bayes_opt2 = bayes_opt_svmrbf(
      folds = fr_rcp_filter_step_pca_0.5,
      bounds = list(.sigma = c(0.00001, 0.1), .C = c(0.00001, 64)),
      init_grid_dt = m_svmrbf_bayes_opt1$History,
      n_iter = 5
    )
  )

plans$p10_final_models <-
  drake::drake_plan(
    m_final_rf = final_model("rf", rcp_pure, data = d$train),
    m_final_glm = final_model("glm", rcp_filter_step_corr_0.7, data = d$train),
    m_final_svm = final_model("svmRadial", rcp_filter_step_pca_0.5, data = d$train, tuneGrid = data.frame(.sigma = 0.06169672, .C = 11.04782256))
  )

plans
