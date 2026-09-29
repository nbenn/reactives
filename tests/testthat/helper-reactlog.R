reactlog_entries <- function(expr) {

  opts <- options(shiny.reactlog = TRUE)
  on.exit(options(opts))

  shiny::reactlogReset()
  force(expr)

  shiny::reactlog()
}

reactlog_labels <- function(expr) {

  log <- reactlog_entries(expr)
  defined <- vapply(log, `[[`, character(1L), "action") == "define"

  vapply(log[defined], `[[`, character(1L), "label")
}

reactlog_writes <- function(expr) {

  log <- reactlog_entries(expr)
  action <- vapply(log, `[[`, character(1L), "action")

  defined <- log[action == "define"]
  changed <- log[action == "valueChange"]

  ids <- vapply(defined, `[[`, character(1L), "reactId")
  labels <- vapply(defined, `[[`, character(1L), "label")

  labels[match(vapply(changed, `[[`, character(1L), "reactId"), ids)]
}
