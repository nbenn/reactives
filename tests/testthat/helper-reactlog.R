reactlog_labels <- function(expr) {

  opts <- options(shiny.reactlog = TRUE)
  on.exit(options(opts))

  shiny::reactlogReset()
  force(expr)

  log <- shiny::reactlog()
  defined <- vapply(log, `[[`, character(1L), "action") == "define"

  vapply(log[defined], `[[`, character(1L), "label")
}
