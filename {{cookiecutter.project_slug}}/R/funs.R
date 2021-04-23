#-# define your functions here


#-# helper-functions

#' Send a msg via pushbullet
#'
#' @param msg string. message to be send
#' @param title string. title for the message. will also be send
#'
#' @return no explicit return value
#' @export
h_send_pushbullet <- function(msg, title = NULL) {
  
  # pushbullet notifications can only be send
  # if ~/.rpushbullet.json exist
  # should follow
  #{
  #  "key": "..key.."
  #}
  #RPushbullet::pbSetup("..key..") can also be used
  #to generate the json-file.
  if (interactive()) {
    return()
  }
  
  if (is.null(title)) {
    title = glue::glue("{Sys.info()['nodename']}: {basename(getwd())}")
  }
  
  try(RPushbullet::pbPost(
    type = "note", 
    title = title, 
    body = glue::glue("{format(Sys.time(), '%Y-%m-%d %H:%M')}\n{msg}")
  ))
}

#' Send a plot via pushbullet
#'
#' @param p ggplot2. plot to be send
#' @param title string. title for the plot. will also be send
#'
#' @return no explicit return value
#' @export
h_send_plot_pushbullet <- function(p, title = NULL){
  # pushbullet notifications can only be send
  # if ~/.rpushbullet.json exist
  # should follow
  #{
  #  "key": "..key.."
  #}
  #RPushbullet::pbSetup("..key..") can also be used
  #to generate the json-file.

  if (interactive()) {
    return()
  }
  
  if (is.null(title)) {
    title = glue::glue("{Sys.info()['nodename']}: {basename(getwd())}")
  }
  
  fName <- glue::glue("{tempfile()}.jpg")
  ggplot2::ggsave(fName, plot = p)
  try(RPushbullet::pbPost(
    type = "file",
    body = title, # title parameter does not work but body at the top and looks quite like title.
    url = fName)
  )
}


#' Converts all column names to lower case
#'
#' @param df data.frame. Error is thrown if lower case names are not unique.
#'
#' @return df but all column names have lower case
#' @export
h_lowercase_names <- function(df) {
  lc <- tolower(names(df))
  if (anyDuplicated(lc) > 0) {
    stop("lowercase varnames makes names ambiguous")
  }
  names(df) <- lc
  df
}


#' Logs the function call with its arguments and a prefix 'start'
#'
#' @param send_pushbullet logical. If true the log-message
#'   also gets send via pushbullet
#'
#' @return no explicit return value
#' @examples
#' f <- function(x) {
#'   h_log_start()
#' }
#' f(x = 4)
h_log_start <- function(send_pushbullet = FALSE) {
  mc <- sys.call(sys.parent())
  mc <- capture.output(print(mc))
  mc <- paste0(trimws(mc), collapse = " ")
  
  msg <- glue::glue("start {mc}")
  if (send_pushbullet) {
    h_send_pushbullet(msg)
  }
  logger::log_info(msg)
}

#' Logs the function call with its arguments and a prefix 'end'
#'
#' @param send_pushbullet logical. If true the log-message
#'   also gets send via pushbullet
#'
#' @return no explicit return value
#' @examples
#' f <- function(x) {
#'   h_log_end)
#' }
#' f(x = 4)
h_log_end <- function(send_pushbullet = FALSE) {
  mc <- sys.call(sys.parent())
  mc <- capture.output(print(mc))
  mc <- paste0(trimws(mc), collapse = " ")

  msg <- glue::glue("end {mc}")
  if (send_pushbullet) {
    h_send_pushbullet(msg)
  }
}

#' Creates a string for labeling for instance plots
#'
#' @param desc string. used to build the returned label
#' @param author string. used to build the returned label
#' @param analysis_unique_id string. used to build the returned label
#'
#' @return string. desc, author, analysis_unique_id and date glued together
#' @export
#'
#' @examples
#' h_create_caption_txt(
#'   desc = "example", 
#'   author = "Voldemord", 
#'   analysis_unique_id = "927894a2-c921-4273-82e5-c3aeb97d2aab")
h_create_caption_txt <- function(desc, author, analysis_unique_id) {
  glue::glue("{desc}
       {author}
       {analysis_unique_id}
       {date()}")
}

#' Adds a label to ggplot2-plot
#'
#' @param desc string. used to build the returned label
#' @param author string. used to build the returned label
#' @param analysis_unique_id string. used to build the returned label
#'
#' @return ggplot2-plot with additional annotates
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
#'   geom_point() + 
#'   h_plot_info(
#'     desc = "Scatterplot", 
#'     author = "Andy Warhole", 
#'     analysis_unique_id = "927894a2-c921-4273-82e5-c3aeb97d2aab")
h_plot_info <- function(desc, author, analysis_unique_id) {
  ggplot2::labs(caption = h_create_caption_txt(desc, author, analysis_unique_id))
}

#' Sets fontsize for labels created by h_plot_info()
#'
#' @param font.size numeric. size for the font
#'
#' @return ggplot2-plot with modified font-size
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#'   geom_point() +
#'   h_plot_info(
#'     desc = "Scatterplot",
#'     author = "Andy Warhole",
#'     analysis_unique_id = "927894a2-c921-4273-82e5-c3aeb97d2aab") +
#'   h_plot_info_font_size(20)
h_plot_info_font_size <- function(font.size) {
  ggplot2::theme(plot.caption = element_text(size = font.size))
}

#' Helperfunction for h_annotate_with_df()
#'
#' Usually counting with dplyr::count and using expressions
#' like A < 100 | 200 < B, then kable uses html-notation for
#' the OR-operation. This is fixed by this function
#' @param s string. usually the output of knitr::kable()
#'
#' @return s but "&#124;" replaced with " or "
#' @export
h_html_to_human_readable <- function(s) {
  s <- gsub("&#124;", "  or  ", s, fixed = TRUE)
  s
}




#' Adds a data.frame to a ggplot2-plot
#'
#' @param df data.frame that is added to the plot
#' @param position position of the table. available options are
#'   "top-left", "top-right", "bottom-left", "bottom-right"
#' @param size numeric. fontsize of the plot
#'
#' @return ggplot2-plot extended the data.frame
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#'   geom_point() +
#'   h_annotate_with_df(dplyr::count(iris, Species == "setosa" | Species == "virginica"))
h_annotate_with_df <- function(df, position = c("top-left", "top-right", "bottom-left", "bottom-right"), size = 3) {
  df_str <- knitr::kable(df) %>%
    as.character() %>%
    h_html_to_human_readable() %>%
    paste(collapse = "\n")
  
  position <- match.arg(position)
  
  if (position == "top-left") {
    return(ggplot2::annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, label = df_str, family = "mono", size = size))
  } else if (position == "top-right") {
    return(ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, label = df_str, family = "mono", size = size))
  } else if (position == "bottom-left") {
    return(ggplot2::annotate("text", x = -Inf, y = -Inf, hjust = 0, vjust = 0, label = df_str, family = "mono", size = size))
  } else if (position == "bottom-right") {
    return(ggplot2::annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = 0, label = df_str, family = "mono", size = size))
  }
  stop("position unknown")
}
