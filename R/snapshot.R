#' @rdname reactives
#' @export
snapshot_reactives <- function(x) {

  check_collection(x)

  label <- collection_label(sys.call(), class(x))
  slots <- isolate(as.list(x))

  withReactiveDomain(
    NULL,
    build_reactives(lapply(slots, snapshot_slot), class(x), label)
  )
}

snapshot_slot <- function(slot) {

  if (inherits(slot, "reactiveVal")) {
    return(reactiveVal(isolate(slot())))
  }

  res <- tryCatch(
    list(value = isolate(slot())),
    error = function(e) list(error = e)
  )

  if (is.null(res$error)) {
    reactive(res$value)
  } else {
    reactive(stop(res$error))
  }
}
