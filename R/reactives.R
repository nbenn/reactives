#' Keyed collections of reactives
#'
#' A `reactives` object is a keyed collection of shiny reactives, such as
#' [shiny::reactiveVal()] and [shiny::reactive()] objects. Slots can be added,
#' removed, renamed and reordered, and each slot is tracked as its own reactive
#' dependency. The `reactive_vals()` constructor creates a collection whose
#' slots are [shiny::reactiveVal()] objects holding the given values, and which
#' only accepts [shiny::reactiveVal()] slots, so code writing through its slots
#' can rely on them being writable. To test whether an object is a collection
#' of either kind, use `is_reactives()`.
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
#' `x[[length(x) + 1]] <- value`. Assigning with `[<-` binds or removes several
#' slots at once, taking `NULL`, a single reactive, or a list with one element
#' per selected slot.
#'
#' Subscripts follow the rules of the vctrs package, which are stricter than
#' base R's. Positions must be whole numbers within range, negative and
#' positive positions can't be mixed, a logical subscript must be size 1 or
#' match the number of slots, and unknown names, empty strings and missing
#' values are errors. A list assigned with `[<-` is only recycled from size 1.
#' Reading a missing name with `[[` or `$` still returns `NULL`, as on a list.
#'
#' @section Sharing:
#' A collection is shared, like [shiny::reactiveValues()]: after `y <- x`, both
#' names refer to the same collection, and a change made through `$<-`,
#' `[[<-`, `[<-`, `names<-` or `reorder()` is seen by everything holding it.
#' That is what lets one part of an app change a collection while another
#' reacts to it. Subsetting with `[` returns a new collection with its own set
#' of slots, holding the same reactives, and `x[]` copies the whole collection.
#' So `x <- x[order]` gives a reordered copy, which suffices when nothing else
#' holds `x`, while `reorder(x, order)` reorders the shared collection itself.
#' Renaming with `names<-` keeps every slot at its position, as for a list.
#'
#' @section Lifetime:
#' A collection belongs to the session or module it was created in and is
#' destroyed along with it, as a [shiny::reactiveVal()] is. Reading or changing
#' it from another module doesn't tie it to that module, so destroying the
#' module leaves the collection intact. A reactive bound to a slot, however,
#' still belongs to the module it was created in.
#'
#' @section Dependencies:
#' Reading a slot makes the caller depend on that slot alone. The caller
#' re-runs when the slot is bound, replaced or removed, but not when other
#' slots change. This also holds for a slot that does not exist yet, so a
#' reader re-runs once the slot is added. Reading `names()` or `length()`
#' depends on which slots exist and in what order, and `as.list()` depends on
#' that and on every slot. Reordering re-runs readers of `names()`, `length()`,
#' `as.list()` and of slots by position, but not readers of slots by name.
#'
#' Printing, `format()` and `str()` make the caller depend on nothing, and they
#' never call a slot, so they run no computed slot. Outside a reactive
#' consumer, as at the console, `names()` and `length()` work as they would
#' inside [shiny::isolate()] rather than fail.
#'
#' @section Reactlog:
#' In [reactlog](https://rstudio.github.io/reactlog/), a collection is named
#' after the variable it was assigned to when created, as a
#' [shiny::reactiveValues()] object is. After `x <- reactives(a = ...)`, the
#' dependency on slot `a` shows as `x$a`, the one on which slots exist and in
#' what order as `names(x)`, and those on unnamed slots as `x$...1`, `x$...2`
#' and so on. Unnamed slots are numbered in the order they were added rather
#' than by position, so a label survives reordering. The name is read from
#' source references, and a collection created without them, as in an
#' installed package, is named after its class and a number instead, as in
#' `reactives1`.
#'
#' @param ... For `reactives()` and `reactive_vals()`, the slots to hold, named
#'   or unnamed: reactives, with `NULL` entries dropped, for `reactives()`, and
#'   any values, including `NULL`, for `reactive_vals()`. Ignored by
#'   `reorder()`.
#'
#' @return A `reactives` object, or for `reactive_vals()` a `reactive_vals`
#'   object, which is also a `reactives` object. The `reorder()` method returns
#'   `x`, invisibly, and `is_reactives()` returns `TRUE` or `FALSE`.
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
#' # Everything holding `y` sees the new order
#' z <- y
#' reorder(y, c("label", "n"))
#' shiny::isolate(names(z))
#'
#' is_reactives(y)
#'
#' @export
reactives <- function(...) {
  build_reactives(
    list(...),
    "reactives",
    collection_label(sys.call(), "reactives")
  )
}

#' @rdname reactives
#' @export
reactive_vals <- function(...) {
  build_reactives(
    lapply(list(...), reactiveVal),
    c("reactive_vals", "reactives"),
    collection_label(sys.call(), "reactive_vals")
  )
}

#' @param x A `reactives` object, or for `is_reactives()`, any object.
#' @param order The new order, listing every slot exactly once: by position, or
#'   by name when all slots are named.
#'
#' @rdname reactives
#' @export
reorder.reactives <- function(x, order, ...) {

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

#' @rdname reactives
#' @export
is_reactives <- function(x) {
  inherits(x, "reactives")
}

build_reactives <- function(slots, class, label) {

  slots <- slots[!vapply(slots, is.null, logical(1L))]

  nms <- names(slots)

  if (is.null(nms)) {
    nms <- character(length(slots))
  }

  named <- nzchar(nms)

  if (anyDuplicated(nms[named])) {
    abort("Each slot needs a distinct name.", "reactives_duplicate_name")
  }

  res <- new_reactives(class, label)

  for (i in seq_along(slots)) {
    key <- if (named[[i]]) nms[[i]] else next_positional_key(res)
    bind_slot(res, key, slots[[i]])
  }

  res
}

# The collection keeps one `reactiveVal()` cell per key, holding the slot's
# reactive or `NULL` when the key has no slot, plus an ordered `keys`
# `reactiveVal()` that is the only record of which slots exist. Reading a key
# subscribes to its cell alone, even when the key has no slot yet, so cells
# are created on first use and are never deleted: a reader subscribed before a
# removal must still re-run when the key comes back. Since shiny destroys a
# reactive along with the module it was created in, cells are created in the
# collection's own reactive domain, whichever module first uses them.
new_reactives <- function(class, label) {

  state <- new.env(parent = emptyenv())
  state$positions <- 0L

  structure(
    list(
      cells = new.env(parent = emptyenv()),
      keys = reactiveVal(character(), label = paste0("names(", label, ")")),
      domain = getDefaultReactiveDomain(),
      label = label,
      state = state
    ),
    class = class
  )
}

raw_keys <- function(x) {
  .subset2(x, "keys")()
}

set_keys <- function(x, keys) {
  .subset2(x, "keys")(keys)
}

# Reading `keys` fails outside a reactive consumer, and shiny exports no way to
# check for one first. Retrying inside `isolate()` lets `names()` and `length()`
# work at the console, while any other error recurs on the retry.
current_keys <- function(x) {
  tryCatch(raw_keys(x), error = function(e) isolate(raw_keys(x)))
}

slot_cell <- function(x, key) {

  cells <- .subset2(x, "cells")
  cell <- cells[[key]]

  if (is.null(cell)) {
    cell <- withReactiveDomain(
      .subset2(x, "domain"),
      reactiveVal(NULL, label = cell_label(x, key))
    )
    assign(key, cell, envir = cells)
  }

  cell
}

get_slot <- function(x, key) {
  slot_cell(x, key)()
}

check_slot <- function(x, value) {

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

  if (inherits(x, "reactive_vals") && !inherits(value, "reactiveVal")) {
    abort(
      paste(
        "A `reactive_vals` collection only holds `reactiveVal()` slots.",
        "Use `reactives()` for a collection that also holds other reactives."
      ),
      "reactives_not_reactive_val"
    )
  }

  invisible(value)
}

bind_slot <- function(x, key, value) {

  check_slot(x, value)

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

#' @export
`$.reactives` <- function(x, name) {
  get_slot(x, name)
}

#' @export
`[[.reactives` <- function(x, i) {

  i <- index2(i)

  if (is.character(i)) {
    return(get_slot(x, i))
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

  i <- index2(i)

  if (is.character(i)) {
    return(assign_slot(x, i, value))
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
  sel <- if (missing(i)) keys else select_keys(keys, i)

  slots <- lapply(sel, get_slot, x = x)
  names(slots) <- replace(sel, is_positional_key(sel), "")

  build_reactives(slots, class(x), collection_label(sys.call(), class(x)))
}

#' @export
`[<-.reactives` <- function(x, i, value) {

  keys <- isolate(raw_keys(x))
  targets <- if (missing(i)) keys else target_keys(keys, i)
  values <- recycle_values(value, length(targets))

  # Check every value before binding any, so that a bad element leaves the
  # collection unchanged.
  for (slot in values[!vapply(values, is.null, logical(1L))]) {
    check_slot(x, slot)
  }

  for (j in seq_along(targets)) {
    assign_slot(x, targets[[j]], values[[j]])
  }

  x
}

#' @export
names.reactives <- function(x) {
  display_names(current_keys(x))
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
  length(current_keys(x))
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

  header <- format_header(x, length(keys))

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

#' @export
# nolint next: object_name_linter.
str.reactives <- function(object, ..., indent.str = " ") {

  keys <- isolate(raw_keys(object))
  slots <- isolate(lapply(keys, get_slot, x = object))

  cat(format_header(object, length(keys)), "\n", sep = "")

  if (length(keys)) {
    cat(
      paste0(
        indent.str,
        "$ ",
        format(replace(keys, is_positional_key(keys), "")),
        ": ",
        vapply(lapply(slots, class), `[[`, character(1L), 1L)
      ),
      sep = "\n"
    )
  }

  invisible()
}

format_header <- function(x, n) {
  sprintf("<%s[%d]>", class(x)[[1L]], n)
}

abort <- function(message, class) {
  stop(
    errorCondition(message, class = c(class, "reactives_error"))
  )
}
