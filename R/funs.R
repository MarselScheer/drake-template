#-# helper-functions

h.send_pushbullet <- function(msg, title = NULL) {
  
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

h.send_plot_pushbullet <- function(p, title = NULL){
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


h.lowercase_names <- function(df) {
  lc <- tolower(names(df))
  if (anyDuplicated(lc) > 0) {
    stop("lowercase varnames makes names ambiguous")
  }
  names(df) <- lc
  df
}


h.log_start <- function(send_pushbullet = FALSE) {
  mc <- sys.call(sys.parent())
  mc <- capture.output(print(mc))
  mc <- paste0(trimws(mc), collapse = " ")
  
  msg <- glue::glue("start {mc}")
  if (send_pushbullet) {
    h.send_pushbullet(msg)
  }
  logger::log_info(msg)
}

h.log_end <- function(send_pushbullet = FALSE) {
  mc <- sys.call(sys.parent())
  mc <- capture.output(print(mc))
  mc <- paste0(trimws(mc), collapse = " ")

  msg <- glue::glue("end {mc}")
  if (send_pushbullet) {
    h.send_pushbullet(msg)
  }
}

h.create_caption_txt <- function(desc, author, analysis_unique_id) {
  glue("{desc}
       {author}
       {analysis_unique_id}
       {date()}")
}

h.plot_info <- function(desc, author, analysis_unique_id) {
  ggplot2::labs(caption = h.create_caption_txt(desc, author, analysis_unique_id))
}

h.plot_info_font_size <- function(font.size) {
  ggplot2::theme(plot.caption = element_text(size = font.size))
}

h.html_to_human_readable <- function(s) {
  s <- gsub("&#124;", "  or  ", s, fixed = TRUE)
  s
}

h.annotate_with_df <- function(df, position = c("top-left", "top-right", "bottom-left", "bottom-right"), size = 3) {
  df_str <- knitr::kable(df) %>%
    as.character() %>%
    h.html_to_human_readable() %>%
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
