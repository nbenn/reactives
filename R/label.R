the <- new.env(parent = emptyenv())
the$labels <- 0L

# As shiny does for `reactiveValues()`, a collection is named after the
# variable it is assigned to, which only the caller's source references show.
# Without them, as in an installed package, the collection is numbered.
collection_label <- function(call, class) {

  label <- assigned_name(attr(call, "srcref", exact = TRUE))

  if (is.null(label)) {
    the$labels <- the$labels + 1L
    label <- paste0(class[[1L]], the$labels)
  }

  label
}

assigned_name <- function(srcref) {

  if (is.null(srcref)) {
    return(NULL)
  }

  line <- as.character(srcref)[[1L]]
  pattern <- "^\\s*([.[:alpha:]][[:alnum:]._$@]*)\\s*(<<-|<-|=(?!=))"
  hit <- regmatches(line, regexec(pattern, line, perl = TRUE))[[1L]]

  if (!length(hit)) {
    return(NULL)
  }

  hit[[2L]]
}

cell_label <- function(x, key) {

  if (is_positional_key(key)) {
    key <- sub(positional_marker(), "...", key, fixed = TRUE)
  }

  paste0(.subset2(x, "label"), "$", key)
}
