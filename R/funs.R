#-# data import

load_fname <- function(fname, .set){
  flog.info(glue::glue("load {fname} and label it as {.set}"))
  suppressMessages(
    readr::read_csv(fname) %>%
      mutate(.set = .set) %>% 
      lowercase_names
  )
}

lowercase_names <- function(df){
  lc <- tolower(names(df))
  if (anyDuplicated(lc) > 0) {
    stop("lowercase varnames makes names ambiguous")
  }
  names(df) <- lc
  df
}


#-# helper-functions

h.add_list_name_as_column <- function(list_of_df){
  lapply(names(list_of_df), function(N){
    list_of_df[[N]]$df_name <- N
    list_of_df[[N]]
  }) %>% 
    dplyr::bind_rows()
}

h.bind_rows_with_id <- function(...) dplyr::bind_rows(..., .id = "target")

h.pj <- function(rcp) recipes::juice(recipes::prep(rcp, retain = TRUE))
h.ad <- as.data.frame
