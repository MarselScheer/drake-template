
# every of the N recipe is trained on every of the K fold and the K trained recipes are stored in N lists
# every of the N trained recipes-lists (containing K trained recipes) is used to fit and assess a glm on the K folds in 
# one step and parameter can be used return the fit or the assessment-set with the predictions

#-# functions

data_preprocess <- function(dt){
  dt$case <- NULL
  dt$cell <- NULL
  dt
}


model_glm <- function(rec, idx, data) {
  purrr::map(seq_along(rec), function(i){
    d <- recipes::bake(rec[[i]], new_data = data[idx[[i]]])
    glm(y ~ ., data = d, family = binomial(), model = F, y = F)
  })
}

to_char <- function(x){
  deparse(substitute(x))
}

auc_glm <- function(rec, rs_idx, data, constellation, return_fit = FALSE, return_assessment_set = FALSE) {
  purrr::map_dfr(seq_along(rec), function(i){
    
    analysis <- recipes::bake(rec[[i]], new_data = data[rs_idx$index[[i]]])
    fit <- parsnip::logistic_reg(mode = "classification", penalty = 0, mixture = 0) %>%
      parsnip::set_engine("glm") %>%
      parsnip::fit(y ~ ., data = analysis)
    
    assessment <- recipes::bake(rec[[i]], new_data = data[rs_idx$indexOut[[i]]]) %>% 
      modelr::add_predictions(fit, type = "prob")
    assessment$pred <- assessment$pred$.pred_WS
    ret <- constellation %>% 
      dplyr::mutate(fold_nmb = i, auc = auc(assessment))

    if (return_fit) {
      ret$fit <- list(fit)
    }
    if (return_assessment_set) {
      ret$assessment <- list(assessment)
    }
    
    ret
  })
}

auc_svm <- function(rec, rs_idx, data, constellation, return_fit = FALSE, return_assessment_set = FALSE) {
  purrr::map_dfr(seq_along(rec), function(i){
    
    analysis <- recipes::bake(rec[[i]], new_data = data[rs_idx$index[[i]]])
    fit <- parsnip::svm_rbf(mode = "classification", cost = !!constellation$cost, rbf_sigma = !!constellation$rbf_sigma) %>%
      parsnip::set_engine("kernlab") %>%
      parsnip::fit(y ~ ., data = analysis)
    
    assessment <- recipes::bake(rec[[i]], new_data = data[rs_idx$indexOut[[i]]]) %>% 
      modelr::add_predictions(fit, type = "prob")
    assessment$pred <- assessment$pred$.pred_WS
    
    ret <- constellation %>% 
      dplyr::mutate(fold_nmb = i, auc = auc(assessment))
    
    if (return_fit) {
      ret$fit <- list(fit)
    }
    if (return_assessment_set) {
      ret$assessment <- list(assessment)
    }
    
    ret
  })
}



auc <- function(assessment) {
  ModelMetrics::auc(assessment$y != "PS", assessment$pred)
}


#-# plans

VFOLDS <- 10
MAX_EXPAND <- NULL

plans <- list()

plans$p00_general <-
  drake_plan(
    rmd_template = rmarkdown::render(knitr_in('presentation_template.Rmd'), output_file = file_out('./reports/00_template.html'), quiet = TRUE),
    AUTHOR = paste("Marsel Scheer"),
    ANALYSIS_UNIQUE_ID = paste("0e2278ab-9bc5-4018-90e9-3500fddc0926"),
    ABSTRACT = paste("Lorem ipsum dolor sit amet, consetetur sadipscing elitr")
  )

plans$p01_rss <- drake_plan(
  max_expand = MAX_EXPAND,
  
  r = get_segmentation_data() %>% h.lowercase_names() %>% dplyr::rename(y = class) %>% data.table::as.data.table(),
  
  initial_split = caret::createDataPartition(y = r$y, times = 1, p = 0.8)$Resample1,
  train  = r[initial_split] %>% data_preprocess(),
  test   = r[-initial_split] %>% dplyr::select(-y) %>% data_preprocess(),
  test_y = r[-initial_split]$y,
  
  rs_idx = rsample::vfold_cv(train, v = VFOLDS, repeats = 1, strata = "y") %>% rsample::rsample2caret(),
  
  trace = TRUE
)

plans$p02_glm_svm <- drake_plan(
  max_expand = MAX_EXPAND,
  
  rcp = target(
    recipes::recipe(y ~ ., data = head(train)) %>%
      step_YeoJohnson(all_predictors()) %>%
      step_center(all_predictors()) %>%
      step_scale(all_predictors()) %>% 
      filter(all_predictors(), threshold = threshold),
    transform = cross(filter = c(step_corr, step_pca), 
                      threshold = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99))
  ),
  
  rec1 = target(
    purrr::map(seq_len(VFOLDS), function(fold_nmb){
      rcp %>% 
        recipes::prep(training = train[rs_idx$index[[fold_nmb]]], retain = FALSE)
    }),
    transform = map(rcp)
  ),
  
  a_glm1 = target(
    auc_glm(rec1, rs_idx, train, 
              constellation = dplyr::tibble(rcp = to_char(rec1),
                            filter = to_char(filter),
                            threshold = threshold)),
    transform = map(rec1)
  ),


  profile_glm1 = target(
    dplyr::bind_rows(a_glm1),
    transform = combine(a_glm1)
  ),
  plot_profile_glm1 = profile_glm1 %>%
    dplyr::select(threshold, auc, fold_nmb, filter) %>% 
    ggplot(aes(x = threshold, y = auc)) +
    geom_line(aes(group = fold_nmb)) +
    facet_wrap(~filter),
  
  
  #-# svm  - add to this plan because we reuse the recipes for the glm
  
    a_svm1 = target(
      auc_svm(rec1, rs_idx, train,
             constellation = dplyr::tibble(rcp = to_char(rec1),
                                           filter = to_char(filter),
                                           threshold = threshold,
                                           rbf_sigma = rbf_sigma,
                                           cost = cost)),
      transform = map(rec1, rbf_sigma = c(0.01), cost = c(1)) #2^0
    ),
    profile_svm1 = target(
      dplyr::bind_rows(a_svm1),
      transform = combine(a_svm1)
    ),
    plot_profile_svm1 = profile_svm1 %>%
      dplyr::select(threshold, filter, cost, rbf_sigma, auc, fold_nmb) %>%
      ggplot(aes(x = threshold, y = auc, color = as.factor(rbf_sigma))) +
      geom_line(aes(group = fold_nmb)) +
      facet_wrap(~filter+cost, labeller = label_both),
  
  
  
  
  
  
  
  
  
  
  
  
  trace = TRUE
);plans$p02_glm_svm %>% dplyr::select(target, command) %>% tail(n = 2); try(plans$p02_glm_svm %>% drake_config() %>% vis_drake_graph(targets_only = TRUE))


auc_parsnip <- function(rec, parsnip_model, constellation, rs_idx, data, return_fit = FALSE, return_assessment_set = FALSE) {
  purrr::map_dfr(seq_along(rec), function(i){
    
    analysis <- recipes::bake(rec[[i]], new_data = data[rs_idx$index[[i]]])
    
    parameters <- constellation
    class(parameters) <- c(class(parameters), "param_grid")
    fit <- parsnip_model %>% 
      merge(parameters) %>% 
      purrr::pluck(1) %>% 
      parsnip::fit(y ~ ., data = analysis)
    
    assessment <- recipes::bake(rec[[i]], new_data = data[rs_idx$indexOut[[i]]]) %>% 
      modelr::add_predictions(fit, type = "prob")
    assessment$pred <- assessment$pred$.pred_WS
    ret <- constellation %>% 
      dplyr::mutate(fold_nmb = i, auc = auc(assessment))
    
    if (return_fit) {
      ret$fit <- list(fit)
    }
    if (return_assessment_set) {
      ret$assessment <- list(assessment)
    }
    
    ret
  })
}


plans$p02_rf <- drake_plan(
  max_expand = MAX_EXPAND,
  
  rcp_rf = target(
    recipes::recipe(y ~ ., data = head(train))
  ),
  
  rec_rf1 = target(
    purrr::map(seq_len(VFOLDS), function(fold_nmb){
      rcp_rf %>% 
        recipes::prep(training = train[rs_idx$index[[fold_nmb]]], retain = FALSE)
    })
  ),
  
  a_rf1 = target(
    { 
      set.seed(20190620)
      auc_parsnip(
        rec = rec_rf1, 
        constellation = dplyr::tibble(rcp = to_char(rec1),
                                      mtry = mtry,
                                      trees = trees),
        parsnip_model = parsnip::rand_forest(mtry = parsnip::varying(), trees = parsnip::varying()) %>%
          parsnip::set_engine("randomForest"),
        rs_idx = rs_idx,
        data = train)
    },
    transform = cross(rec_rf1, mtry = c(5,10,20,30), trees = c(100,200))
  ),
  
  profile_rf1 = target(
    dplyr::bind_rows(a_rf1),
    transform = combine(a_rf1)
  ),
  plot_profile_rf1 = profile_rf1 %>%
    dplyr::select(mtry, auc, fold_nmb, trees) %>% 
    ggplot(aes(x = mtry, y = auc)) +
    geom_line(aes(group = fold_nmb)) + 
    facet_wrap(~trees),
  
  trace = TRUE
);plans$p02_rf %>% dplyr::select(target, command) %>% tail(n = 20); try(plans$p02_rf %>% drake_config() %>% vis_drake_graph(targets_only = TRUE))



bayes_opt_svm <- function(rec, rs_idx, data, bounds, init_grid_dt = NULL, init_points = 0, n_iter) {
  h.log_start()
  
  FUN <- function(rbf_sigma, cost) {
    constellation <- data.frame(rbf_sigma = rbf_sigma, cost = cost)
    txt <- capture.output(
      result <- auc_svm(rec, rs_idx, data, constellation)
    )
    list(Score = mean(result$auc), Pred = 0)
  }

  ret <-
    rBayesianOptimization::BayesianOptimization(
      FUN,
      bounds = bounds,
      init_grid_dt = init_grid_dt,
      init_points = init_points,
      n_iter = n_iter,
      acq = "ucb",
      verbose = TRUE
    )
  
  ret$History <- dplyr::rename(ret$History, auc = Value)
  
  h.log_end()
  ret
}

plans$p02_bo_svm <- drake_plan(
  max_expand = MAX_EXPAND,
  
  bo_svm1 = target(
    bayes_opt_svm(rec = rec1_rcp_step_pca_0.5, 
                  rs_idx = rs_idx, 
                  data = train,
                  bounds = list(rbf_sigma = c(0.00001, 0.1), cost = c(0.00001, 64)), 
                  init_grid_dt = NULL, 
                  init_points = 10,
                  n_iter = 2
    )
  ),

  bo_svm2 = target(
    bayes_opt_svm(rec = rec1_rcp_step_pca_0.5, 
                  rs_idx = rs_idx, 
                  data = train,
                  bounds = list(rbf_sigma = c(0.00001, 0.1), cost = c(0.00001, 64)), 
                  init_grid_dt = bo_svm1$History %>% dplyr::select(rbf_sigma, cost, Value = auc), 
                  init_points = 0,
                  n_iter = 2
    )
  ),
  
  plot_bo_svm2 = bo_svm2$History %>% 
    ggplot(aes(x = rbf_sigma, y = cost, color = auc, size = auc, shape = auc == max(auc))) + 
    geom_point(),
  
  trace = TRUE
);plans$p02_bo_svm %>% dplyr::select(target, command) %>% tail(n = 2); try(plans$p02_bo_svm %>% drake_config() %>% vis_drake_graph(targets_only = TRUE))

