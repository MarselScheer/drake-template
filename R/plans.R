plans <- list()

get_segmentation_data <- function() {
  data("segmentationData", package = "caret")
  return(segmentationData)
}


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
  drake::bind_plans(
    drake_plan(
      rcp_filter = rcp_basic %>% 
        filter__(all_predictors(), threshold = threshold__)
    ) %>% evaluate_plan(
      rules = list(
        filter__ = "step_corr",
        threshold__ = c((1:9)/10, 0.95, 0.99)
      ),
      trace = TRUE
    ),
    drake_plan(
      rcp_filter = rcp_basic %>% 
        filter__(all_predictors(), threshold = threshold__)
    ) %>% evaluate_plan(
      rules = list(
        filter__ = "step_pca",
        threshold__ = c((1:9)/10, 0.95, 0.99)
      ),
      trace = TRUE
    )  
  ) %>% 
  dplyr::rename(filter = filter__, threshold = threshold__) %>% 
  dplyr::select(-contains("__from"))


plans$p06_folds_1_pure <- 
  drake_plan(
    fr_pure = apply_recipe_to_folds(rcp_pure, raw_folds)
  )



plans$p06_folds_1_filter <- 
  drake_plan(
    fr  = apply_recipe_to_folds(rcp__, raw_folds)
  ) %>% h.insert_base_plan(
    base_plan = plans$p05_recipes_2_filter,
    base_plan_wildcard = "rcp__",
    rules = list(
    ),
    trace = TRUE
  )



plans$p07_model_tuning_1_rf <- 
  drake_plan(
    m = purrr::map_dfr(rcp__, function(fold) metric_profile_per_fold('model__', fold, tune_grid = data.frame(mtry = c(5, 10, 20, 30, 40, 58))), .id = "fold")
  ) %>% 
  h.insert_base_plan(
    base_plan = plans$p06_folds_1_pure,
    base_plan_wildcard = "rcp__",
    rules = list(
      model__ = "rf"
    ),
    trace = TRUE
  ) %>% 
    h.clear__()




plans$p07_model_tuning_1_glm <- 
  drake_plan(
    m = purrr::map_dfr(rcp__, function(fold) metric_profile_per_fold('model__', fold, NULL), .id = "fold")
  ) %>% 
  h.insert_base_plan(
    base_plan = plans$p06_folds_1_filter,
    base_plan_wildcard = "rcp__",
    rules = list(
      model__ = "glm"
    ),
    trace = TRUE
  ) %>% 
  h.clear__()


plans$p07_model_tuning_1_svm <- 
  drake_plan(
    m = purrr::map_dfr(rcp__, function(fold) metric_profile_per_fold('model__', fold, NULL, tune_grid = data.frame(.sigma = c(0.005, 0.01, 0.02), .C = 2^seq(-4,4)), prob.model = TRUE), .id = "fold")
  ) %>% 
  h.insert_base_plan(
    base_plan = plans$p06_folds_1_filter,
    base_plan_wildcard = "rcp__",
    rules = list(
      model__ = "svmRadial"
    ),
    trace = TRUE
  ) %>% 
  h.clear__()


all_model_tuning_plans <- drake::bind_plans(
  plans$p07_model_tuning_1_rf,
  plans$p07_model_tuning_1_glm,
  plans$p07_model_tuning_1_svm)

plans$p08_aggregate <- 
  drake::bind_plans(
    gather_plan(all_model_tuning_plans, target = "pre_gathered_metric", gather = "h.bind_rows_with_id"),
    drake_plan(
      profiles = dplyr::left_join(pre_gathered_metric, dplyr::select(all_model_tuning_plans, -command)),
      plot_rf_profiles = profiles %>% 
        dplyr::filter(model == "rf") %>% 
        dplyr::group_by(mtry) %>% 
        dplyr::mutate(AUC_mean = mean(AUC), AUC_sd = sd(AUC)) %>% 
        ggplot(aes(y = AUC, x = mtry)) + 
        geom_line(aes(group = fold), alpha = 0.2) + 
        geom_line(aes(y = AUC_mean)) + 
        geom_errorbar(aes(ymin = AUC_mean - AUC_sd, ymax = AUC_mean + AUC_sd)),
      plot_svmrbf_profiles = profiles %>% 
        dplyr::filter(model == "svmRadial") %>% 
        dplyr::group_by(filter, threshold, .sigma, .C) %>% 
        dplyr::mutate(AUC_mean = mean(AUC), AUC_sd = sd(AUC)) %>% 
        ggplot(aes(y = AUC, x = threshold, group = interaction(.C), color = interaction(.C))) + 
        facet_grid( .sigma ~ filter, labeller = label_both) + 
        geom_line(aes(y = AUC_mean)),
      plot_glm_profiles = profiles %>% 
        filter(model == "glm") %>% 
        dplyr::group_by(filter, threshold) %>% 
        dplyr::mutate(AUC_mean = mean(AUC), AUC_sd = sd(AUC)) %>% 
        ggplot(aes(y = AUC, x = threshold, color = filter)) + 
        geom_line(aes(group = interaction(fold, filter), color = filter), alpha = 0.2) +
        geom_line(aes(y = AUC_mean, group = filter)) + 
        geom_errorbar(aes(ymin = AUC_mean - AUC_sd, ymax = AUC_mean + AUC_sd, group = filter))
    )
  )
  
plans$p09_bayes_opt <- 
  drake::drake_plan(
    m_svmrbf_bayes_opt1 = bayes_opt_svmrbf(
      folds = fr_rcp_filter_step_pca_0.5, 
      bounds = list(.sigma = c(0.00001, 0.1), .C = c(0.00001, 64)), 
      init_grid_dt = m_svmRadial_fr_rcp_filter_step_pca_0.5, 
      n_iter = 5),
    m_svmrbf_bayes_opt2 = bayes_opt_svmrbf(
      folds = fr_rcp_filter_step_pca_0.5, 
      bounds = list(.sigma = c(0.00001, 0.1), .C = c(0.00001, 64)), 
      init_grid_dt = m_svmrbf_bayes_opt1$History, 
      n_iter = 5)
  )
plans

