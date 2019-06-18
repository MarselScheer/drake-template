
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
    fit <- glm(y ~ ., data = analysis, family = binomial())

    assessment <- recipes::bake(rec[[i]], new_data = data[rs_idx$indexOut[[i]]]) %>% 
      modelr::add_predictions(fit)

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

plans$p02_glm <- drake_plan(
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
    transform = cross(rcp)
  ),
  
  a_glm1 = target(
    auc_glm(rec1, rs_idx, train, 
              constellation = dplyr::tibble(model = to_char(rec1),
                            filter = to_char(filter),
                            threshold = threshold)),
    transform = map(rec1)
  ),


  profile_glm1 = target(
    dplyr::bind_rows(a_glm1),
    transform = combine(a_glm1)
  ),
  plot_profile_glm1 = profile_glm1 %>%
    ggplot(aes(x = threshold, y = auc)) +
    geom_line(aes(group = fold_nmb)) +
    facet_wrap(~filter),
  
  trace = TRUE
);plans$p02_glm %>% dplyr::select(target, command) %>% tail(n = 2); plans$p02_glm %>% drake_config() %>% vis_drake_graph(targets_only = TRUE)
