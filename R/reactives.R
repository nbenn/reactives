#' Keyed collections of reactives
#'
#' A `reactives` object is a keyed collection of shiny reactives, such as
#' [shiny::reactiveVal()] and [shiny::reactive()] objects. Slots can be added,
#' removed, renamed and reordered, and each slot is tracked as its own reactive
#' dependency. The `reactive_vals()` constructor creates a collection whose
#' slots are [shiny::reactiveVal()] objects holding the given values.
#'
#' Reading a slot, with `x$a`, `x[["a"]]` or `x[[1]]`, returns the slot's
#' reactive, or `NULL` if there is no such slot, as `$` does on a list. Call
#' the result to get its value, as in `x$a()`. Because a slot holds a reactive
#' rather than a value, a slot whose [shiny::reactiveVal()] stores `NULL` is
#' distinct from a missing slot.
#'
#' Assigning a reactive to a slot binds it, and assigning `NULL` removes the
#' slot, again as for a list. Assigning anything else is an error: a value is
#' written through the slot's reactive instead, as in `x$a(value)` for a
#' [shiny::reactiveVal()] slot. Slots can be unnamed; append one with
#' `x[[length(x) + 1]] <- value`. Subsetting with `[` returns a new collection
#' holding the same reactives.
#'
#' The `reorder_reactives()` function changes the order of the slots in place.
#' Readers of `names()`, `length()` and `as.list()` re-run, as do readers of
#' slots by position; readers of slots by name do not.
#'
#' @section Dependencies:
#' Reading a slot makes the caller depend on that slot alone. The caller
#' re-runs when the slot is bound, replaced or removed, but not when other
#' slots change. This also holds for a slot that does not exist yet, so a
#' reader re-runs once the slot is added. Reading `names()` or `length()`
#' depends on which slots exist and in what order, and `as.list()` depends on
#' that and on every slot.
#'
#' @param ... Reactives to hold, named or unnamed. For `reactives()`, `NULL`
#'   entries are dropped. For `reactive_vals()`, any values, including `NULL`.
#'
#' @return A `reactives` object; `reorder_reactives()` returns `x`, invisibly.
#'
#' @examples
#' x <- reactives(a = shiny::reactiveVal(1), b = shiny::reactive(2 * 21))
#' shiny::isolate(x$a())
#' shiny::isolate(x$b())
#'
#' # A stored NULL is not a missing slot
#' x$c <- shiny::reactiveVal(NULL)
#' shiny::isolate(is.null(x$c))
#' shiny::isolate(is.null(x$d))
#'
#' # Assigning NULL removes a slot
#' x$a <- NULL
#' shiny::isolate(names(x))
#'
#' y <- reactive_vals(n = 1, label = "one")
#' shiny::isolate(y$label())
#'
#' reorder_reactives(y, c("label", "n"))
#' shiny::isolate(names(y))
#'
#' @export
reactives <- function(...) {

  slots <- list(...)
  slots <- slots[!vapply(slots, is.null, logical(1L))]

  nms <- names(slots)

  if (is.null(nms)) {
    nms <- character(length(slots))
  }

  named <- nzchar(nms)

  if (anyDuplicated(nms[named])) {
    abort("Each slot needs a distinct name.", "reactives_duplicate_name")
  }

  res <- new_reactives()

  for (i in seq_along(slots)) {
    key <- if (named[[i]]) nms[[i]] else next_positional_key(res)
    bind_slot(res, key, slots[[i]])
  }

  res
}

#' @rdname reactives
#' @export
reactive_vals <- function(...) {
  do.call(reactives, lapply(list(...), reactiveVal))
}

#' @param x A `reactives` object.
#' @param order The new order, listing every slot exactly once: by position, or
#'   by name when all slots are named.
#'
#' @rdname reactives
#' @export
reorder_reactives <- function(x, order) {

  if (!inherits(x, "reactives")) {
    abort("Expected a `reactives` object.", "reactives_bad_object")
  }

  keys <- isolate(raw_keys(x))
  new_keys <- if (is.numeric(order)) keys[order] else order

  valid <- length(new_keys) == length(keys) && !anyNA(new_keys) &&
    setequal(new_keys, keys) && !anyDuplicated(new_keys)

  if (!valid) {
    abort(
      paste(
        "The order must list every slot exactly once, by position, or by",
        "name when all slots are named."
      ),
      "reactives_bad_order"
    )
  }

  set_keys(x, new_keys)

  invisible(x)
}

# The collection keeps one `reactiveVal()` cell per key, holding the slot's
# reactive or `NULL` when the key has no slot, plus an ordered `keys`
# `reactiveVal()` that is the only record of which slots exist. Reading a key
# subscribes to its cell alone, even when the key has no slot yet, so cells
# are created on first use and are never deleted: a reader subscribed before a
# removal must still re-run when the key comes back.
new_reactives <- function() {

  state <- new.env(parent = emptyenv())
  state$positions <- 0L

  structure(
    list(
      cells = new.env(parent = emptyenv()),
      keys = reactiveVal(character()),
      state = state
    ),
    class = "reactives"
  )
}

raw_keys <- function(x) {
  .subset2(x, "keys")()
}

set_keys <- function(x, keys) {
  .subset2(x, "keys")(keys)
}

slot_cell <- function(x, key) {

  cells <- .subset2(x, "cells")
  cell <- cells[[key]]

  if (is.null(cell)) {
    cell <- reactiveVal(NULL)
    assign(key, cell, envir = cells)
  }

  cell
}

get_slot <- function(x, key) {
  slot_cell(x, key)()
}

bind_slot <- function(x, key, value) {

  if (!is.reactive(value)) {
    abort(
      paste(
        "A slot holds a reactive, such as `reactiveVal()` or `reactive()`.",
        "Assign `NULL` to remove a slot, or write a value through the",
        "slot's reactive."
      ),
      "reactives_not_reactive"
    )
  }

  slot_cell(x, key)(value)

  keys <- isolate(raw_keys(x))

  if (!key %in% keys) {
    set_keys(x, c(keys, key))
  }

  invisible(x)
}

remove_slot <- function(x, key) {

  keys <- isolate(raw_keys(x))

  if (key %in% keys) {
    slot_cell(x, key)(NULL)
    set_keys(x, setdiff(keys, key))
  }

  invisible(x)
}

assign_slot <- function(x, key, value) {

  if (is.null(value)) {
    remove_slot(x, key)
  } else {
    bind_slot(x, key, value)
  }

  x
}

# An unnamed slot still needs a key. It gets a hidden synthetic one, prefixed
# with a control character so that `names()` can blank it while a real name,
# even "1", survives. Positional keys are never reused.
positional_marker <- function() {
  intToUtf8(1L)
}

next_positional_key <- function(x) {

  state <- .subset2(x, "state")
  state$positions <- state$positions + 1L
  paste0(positional_marker(), state$positions)
}

is_positional_key <- function(keys) {
  startsWith(keys, positional_marker())
}

display_names <- function(keys) {

  pos <- is_positional_key(keys)

  if (all(pos)) {
    return(NULL)
  }

  replace(keys, pos, "")
}

is_name <- function(i) {
  is.character(i) && length(i) == 1L && !is.na(i) && nzchar(i)
}

is_position <- function(i) {
  is.numeric(i) && length(i) == 1L && !is.na(i) && i >= 1 && i == trunc(i)
}

bad_index <- function() {
  abort(
    "A slot is indexed by a single name or position.",
    "reactives_bad_index"
  )
}

out_of_bounds <- function(i, n) {
  abort(
    sprintf("Position %d is beyond the %d slot(s).", i, n),
    "reactives_out_of_bounds"
  )
}

#' @export
`$.reactives` <- function(x, name) {
  get_slot(x, name)
}

#' @export
`[[.reactives` <- function(x, i) {

  if (is_name(i)) {
    return(get_slot(x, i))
  }

  if (!is_position(i)) {
    bad_index()
  }

  keys <- raw_keys(x)

  if (i > length(keys)) {
    out_of_bounds(i, length(keys))
  }

  get_slot(x, keys[[i]])
}

#' @export
`$<-.reactives` <- function(x, name, value) {
  assign_slot(x, name, value)
}

#' @export
`[[<-.reactives` <- function(x, i, value) {

  if (is_name(i)) {
    return(assign_slot(x, i, value))
  }

  if (!is_position(i)) {
    bad_index()
  }

  keys <- isolate(raw_keys(x))

  if (i == length(keys) + 1L) {

    if (!is.null(value)) {
      bind_slot(x, next_positional_key(x), value)
    }

    return(x)
  }

  if (i > length(keys)) {
    out_of_bounds(i, length(keys))
  }

  assign_slot(x, keys[[i]], value)
}

#' @export
`[.reactives` <- function(x, i) {

  keys <- raw_keys(x)

  if (is.character(i)) {

    unknown <- setdiff(i, keys[!is_positional_key(keys)])

    if (length(unknown)) {
      abort(
        paste0("Unknown slot names: ", paste(unknown, collapse = ", "), "."),
        "reactives_unknown_name"
      )
    }

    sel <- i

  } else if (is.numeric(i) || is.logical(i)) {

    pos <- seq_along(keys)[i]

    if (anyNA(pos)) {
      abort("Positions must lie within the slots.", "reactives_out_of_bounds")
    }

    sel <- keys[pos]

  } else {
    abort(
      "Slots are selected by names, positions or a logical vector.",
      "reactives_bad_index"
    )
  }

  slots <- lapply(sel, get_slot, x = x)
  names(slots) <- replace(sel, is_positional_key(sel), "")

  do.call(reactives, slots)
}

#' @export
names.reactives <- function(x) {
  display_names(raw_keys(x))
}

#' @export
`names<-.reactives` <- function(x, value) {

  keys <- isolate(raw_keys(x))

  if (is.null(value)) {
    value <- character(length(keys))
  }

  value <- as.character(value)

  if (length(value) != length(keys) || anyNA(value)) {
    abort(
      "Supply one name per slot, using \"\" for an unnamed slot.",
      "reactives_bad_names"
    )
  }

  named <- nzchar(value)

  if (anyDuplicated(value[named])) {
    abort("Each slot needs a distinct name.", "reactives_duplicate_name")
  }

  new_keys <- keys
  new_keys[named] <- value[named]

  unnamed <- which(!named & !is_positional_key(keys))

  for (j in unnamed) {
    new_keys[[j]] <- next_positional_key(x)
  }

  moved <- new_keys != keys

  if (!any(moved)) {
    return(x)
  }

  # Read every moving slot before writing any cell, so that swapping two names
  # does not overwrite a slot before it has moved.
  slots <- isolate(lapply(keys[moved], get_slot, x = x))

  for (key in setdiff(keys, new_keys)) {
    slot_cell(x, key)(NULL)
  }

  for (j in seq_along(slots)) {
    slot_cell(x, new_keys[moved][[j]])(slots[[j]])
  }

  set_keys(x, new_keys)

  x
}

#' @export
length.reactives <- function(x) {
  length(raw_keys(x))
}

#' @export
as.list.reactives <- function(x, ...) {

  keys <- raw_keys(x)

  res <- lapply(keys, get_slot, x = x)
  names(res) <- display_names(keys)

  res
}

#' @export
format.reactives <- function(x, ...) {

  keys <- isolate(raw_keys(x))

  labels <- ifelse(
    is_positional_key(keys),
    paste0("[[", seq_along(keys), "]]"),
    paste0("$", keys)
  )

  header <- sprintf("<reactives[%d]>", length(keys))

  if (!length(keys)) {
    return(header)
  }

  c(header, paste0("  ", labels))
}

#' @export
print.reactives <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

abort <- function(message, class) {
  stop(
    errorCondition(message, class = c(class, "reactives_error"))
  )
}
