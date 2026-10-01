#' Keyed collections of reactives
#'
#' A `reactives` object is a keyed collection of shiny reactives, such as
#' [shiny::reactiveVal()] and [shiny::reactive()] objects. Slots can be added,
#' removed and reordered, and each slot is tracked as its own reactive
#' dependency. The `reactive_vals()` constructor creates a collection whose
#' slots are [shiny::reactiveVal()] objects holding the given values, and which
#' only accepts [shiny::reactiveVal()] slots, so code writing through its slots
#' can rely on them being writable. To test whether an object is a collection
#' of either kind, use `is_reactives()`.
#'
#' A collection has two levels, as a list does: the slots, and the values
#' their reactives return. Reading with `$` or `[[`, as in `x$a`, `x[["a"]]` or
#' `x[[1]]`, returns a slot's value, as it does for a [shiny::reactiveValues()]
#' object, while reading with `[`, as in `x["a"]` or `x[1]`, returns the slot's
#' reactive. As on a list, a name with no slot reads as `NULL`, while a
#' position past the last slot is an error. A slot whose [shiny::reactiveVal()]
#' stores `NULL` reads as `NULL` with `$` and `[[` too, but not with `[`, which
#' tells it apart from a missing slot. To test for slots by name, use
#' `has_key()`.
#'
#' Each read takes a single name or position. Take the reactives of several
#' slots from `as.list(x)[i]` instead, or get the value of every slot at once
#' with `as_values()`, as [shiny::reactiveValuesToList()] does for a
#' [shiny::reactiveValues()] object. Going through the slots' reactives works
#' with `lapply()` and `vapply()`, which call `as.list()`, while `Map()` reads
#' the slots by position with `[[` and so goes through their values. A `for`
#' loop, `do.call()` and purrr's `map()` functions don't dispatch on the class,
#' though, so they see the collection's internal fields instead of its slots.
#' With those, go through `as.list(x)` instead, as in
#' `for (slot in as.list(x))`.
#'
#' Assigning a reactive with `[<-`, as in `x["a"] <- r`, binds it to the slot,
#' adding the slot or replacing its reactive, and assigning `NULL` with `[<-`
#' removes the slot, as for a list. Assigning anything else with `[<-` is an
#' error. Assigning with `$<-` or `[[<-` writes a value instead, as for a
#' [shiny::reactiveValues()] object: through the slot's [shiny::reactiveVal()],
#' which stays bound, or where there is no slot, by binding a new
#' [shiny::reactiveVal()] that holds the value. Any value is stored as it is, a
#' reactive included, and unlike on a list, assigning `NULL` with `$<-` or
#' `[[<-` stores `NULL` rather than removing the slot. A slot holding a
#' reactive expression, such as a [shiny::reactive()], can't be written, so
#' that a stray value can't overwrite it; replacing its reactive takes `[<-`.
#' As with reading, each assignment takes a single name or position. Slots can
#' be unnamed; append one with `x[length(x) + 1] <- r` for a reactive `r`, or
#' with `x[[length(x) + 1]] <- value` for a value.
#'
#' Renaming with `names<-` is an error, as it is for a
#' [shiny::reactiveValues()] object. To rename a slot, bind its reactive under
#' the new name, remove the old slot and restore the order with `reorder()`.
#'
#' @section Sharing:
#' A collection is shared, like [shiny::reactiveValues()]: after `y <- x`, both
#' names refer to the same collection, and a change made through `[<-`, `$<-`,
#' `[[<-` or `reorder()` is seen by everything holding it. That is what lets
#' one part of an app change a collection while another reacts to it.
#'
#' @section Lifetime:
#' A collection belongs to the session or module it was created in and is
#' destroyed along with it, as a [shiny::reactiveVal()] is. Reading or changing
#' it from another module doesn't tie it to that module, so destroying the
#' module leaves the collection intact. A reactive bound to a slot, however,
#' still belongs to the module it was created in, while the
#' [shiny::reactiveVal()] that `$<-` or `[[<-` creates for a new slot belongs to
#' the collection, whichever module writes the value.
#'
#' To keep reading a collection after its [shiny::testServer()] session has
#' closed, a test can take a snapshot with `snapshot_reactives()` while the
#' session is still open. A snapshot belongs to no session. For each
#' [shiny::reactiveVal()] slot it holds a new [shiny::reactiveVal()] with the
#' slot's current value, and for each other slot a [shiny::reactive()]
#' returning its current value, so writing through a slot of the snapshot or
#' of the original leaves the other unchanged. A slot that fails when computed
#' fails the same way when its copy is called, rather than failing the
#' snapshot. Names, order and class carry over.
#'
#' @section Dependencies:
#' Reading a slot's reactive by name, as in `x["a"]`, makes the caller depend
#' on that slot alone. The caller re-runs when the slot is bound, replaced or
#' removed, but not when other slots change. This also holds for a slot that
#' does not exist yet, so a reader re-runs once the slot is added. Reading the
#' slot's value, as in `x$a` or `x[["a"]]`, depends on the slot and on its
#' value, as calling its reactive does, so the caller also re-runs when the
#' value changes. Testing for slots with `has_key()` has the same
#' dependencies as reading their reactives by name, while testing with
#' `"a" %in% names(x)` depends on what `names()` depends on, so the caller
#' re-runs whenever a slot is added, removed or moved.
#'
#' The `length()` method depends on the number of slots alone, and `names()` on
#' which slots exist and their order. Reading a slot by position, as in `x[1]`
#' or `x[[1]]`, depends on what `names()` depends on and on the slot it finds,
#' and `x[[1]]` also on its value, so the caller re-runs whenever a slot is
#' added, removed or moved, even if the slot at its position stays the same.
#' Reading past the last slot fails, and the caller re-runs on the same
#' changes. The `as.list()` method depends on what `names()` depends on and on
#' every slot, and `as_values()` also on every slot's value. Reordering
#' re-runs readers of `names()`, `as.list()`, `as_values()` and of slots by
#' position, but not readers of `length()` or of slots by name.
#'
#' To depend on the slot at a position alone, read it through a
#' [shiny::reactiveVal()]. After `first <- reactiveVal()` and
#' `observe(first(if (length(x)) x[1]))`, a reader of `first()` re-runs only
#' when position 1 comes to hold a different reactive or none, since writing a
#' [shiny::reactiveVal()] the value it already holds invalidates nothing. The
#' check on `length()` keeps the observer from failing on an empty collection,
#' which in an app would end the session.
#'
#' Binding, removing and writing make the caller depend on nothing. Writing a
#' value through a slot's [shiny::reactiveVal()] leaves the slot bound to it,
#' so the write re-runs only the readers of the slot's value, and only if the
#' value changes. Writing a value where there is no slot binds a new one and
#' re-runs readers as any binding does.
#'
#' Printing, `format()` and `str()` make the caller depend on nothing, and they
#' never call a slot, so they run no computed slot. Taking a snapshot also
#' makes the caller depend on nothing, though it runs every computed slot.
#' Outside a reactive consumer, as at the console, `names()` and `length()`
#' work as they would inside [shiny::isolate()] rather than fail.
#'
#' @section Reactlog:
#' In [reactlog](https://rstudio.github.io/reactlog/), the dependency on slot
#' `a` shows as `reactives$a`, or as `reactive_vals$a` in a `reactive_vals`
#' collection, the one on which slots exist and in what order as
#' `names(reactives)`, and the one on their number as `length(reactives)`.
#' Unnamed slots show as `reactives$...1`, `reactives$...2` and so on, numbered
#' in the order they were added rather than by position, so a label survives
#' reordering. A read of every slot at once, such as `as.list()`, shows a
#' single dependency on `reactives[]` instead of one per slot. The entry for a
#' slot only appears once it has been read, and the one for the number of
#' slots once `length()` has been called.
#'
#' Each [shiny::reactiveVal()] that `reactive_vals()`, `$<-` or `[[<-` creates
#' for a value shows as the label of its slot followed by `()`,
#' `reactive_vals$a()` for slot `a` and `reactive_vals$...1()` for the first
#' unnamed slot, and the error for calling it once its module is destroyed
#' names it the same way. The label names the slot the value was created for,
#' and stays with the [shiny::reactiveVal()] if that is later bound to another
#' slot.
#'
#' @param ... For `reactives()` and `reactive_vals()`, the slots to hold, named
#'   or unnamed: reactives, with `NULL` entries dropped, for `reactives()`, and
#'   any values, including `NULL`, for `reactive_vals()`. Ignored by
#'   `reorder()`.
#'
#' @return A `reactives` object, or for `reactive_vals()` a `reactive_vals`
#'   object, which is also a `reactives` object. A snapshot has the class of
#'   `x`. The `reorder()` method returns `x`, invisibly, `as_values()`
#'   returns a list of the slots' values, in slot order and named as by
#'   `as.list()`, `has_key()` returns a logical vector with one element per
#'   name in `key`, and `is_reactives()` returns `TRUE` or `FALSE`.
#'
#' @examples
#' x <- reactives(a = shiny::reactiveVal(1), b = shiny::reactive(2 * 21))
#' shiny::isolate(x$a)
#' shiny::isolate(x[["b"]])
#'
#' # Assigning a value writes it, adding a slot if there is none
#' x$a <- 2
#' x$c <- NULL
#' shiny::isolate(as_values(x))
#'
#' # A stored NULL reads as NULL, but `[` finds the slot's reactive
#' shiny::isolate(is.null(x$c))
#' shiny::isolate(is.null(x["c"]))
#' shiny::isolate(has_key(x, c("c", "d")))
#'
#' # Assigning a reactive with `[<-` binds it, and assigning NULL removes a slot
#' x["d"] <- shiny::reactive(10 * x$a)
#' x["b"] <- NULL
#' shiny::isolate(names(x))
#' shiny::isolate(x$d)
#'
#' y <- reactive_vals(n = 1, label = "one")
#' shiny::isolate(y$label)
#'
#' # Everything holding `y` sees the new order
#' z <- y
#' reorder(y, c("label", "n"))
#' shiny::isolate(names(z))
#'
#' shiny::isolate(as_values(y))
#' is_reactives(y)
#'
#' # A snapshot outlives the session it was taken in
#' snap <- NULL
#' shiny::testServer(
#'   function(input, output, session) {
#'     x <- reactives(n = shiny::reactive(input$n))
#'   },
#'   {
#'     session$setInputs(n = 21)
#'     snap <<- snapshot_reactives(x)
#'   }
#' )
#' shiny::isolate(snap$n)
#'
#' @export
reactives <- function(...) {
  build_reactives(list(...), "reactives")
}

#' @rdname reactives
#' @export
reactive_vals <- function(...) {
  build_reactives(list(...), c("reactive_vals", "reactives"), values = TRUE)
}

#' @param x A `reactives` object, or for `is_reactives()`, any object.
#' @param order The new order, listing every slot exactly once: by position, or
#'   by name when all slots are named.
#'
#' @rdname reactives
#' @export
reorder.reactives <- function(x, order, ...) {

  keys <- live_keys(x)
  positions <- if (is.numeric(order)) order else match(order, keys)

  valid <- length(positions) == length(keys) &&
    setequal(positions, seq_along(keys))

  if (!valid) {
    abort(
      paste(
        "The order must list every slot exactly once, by position, or by",
        "name when all slots are named."
      ),
      "reactives_bad_order"
    )
  }

  set_keys(x, keys[positions])

  invisible(x)
}

#' @rdname reactives
#' @export
is_reactives <- function(x) {
  inherits(x, "reactives")
}

#' @rdname reactives
#' @export
as_values <- function(x) {
  check_collection(x)
  lapply(as.list(x), do.call, list())
}

#' @param key The names of the slots to test for, none of them empty or
#'   missing.
#'
#' @rdname reactives
#' @export
has_key <- function(x, key) {
  check_collection(x)
  !vapply(lapply(subscript_names(key), get_slot, x = x), is.null, logical(1L))
}

build_reactives <- function(slots, class, values = FALSE) {

  if (!values) {
    slots <- slots[!vapply(slots, is.null, logical(1L))]
  }

  keys <- names(slots)

  if (is.null(keys)) {
    keys <- character(length(slots))
  }

  named <- nzchar(keys)

  if (anyDuplicated(keys[named])) {
    abort("Each slot needs a distinct name.", "reactives_duplicate_name")
  }

  res <- new_reactives(class)

  for (i in which(!named)) {
    keys[[i]] <- next_positional_key(res)
  }

  if (values) {
    slots <- Map(reactiveVal, slots, value_label(res, keys))
  } else {
    for (slot in slots) {
      check_slot(res, slot)
    }
  }

  names(slots) <- keys
  list2env(slots, envir = .subset2(res, "slots"))

  set_keys(res, keys)

  res
}

# The slots live in a plain environment, and an ordered `keys` `reactiveVal()`
# records which slots exist, with a plain copy in `state` for methods that
# read the keys without subscribing. Reading a key subscribes to a
# `reactiveVal()` cell for that key alone and then takes the slot from the
# environment. Rather than the slot, which a `reactiveVal()` created with it
# would keep alive for as long as it exists, the cell holds a count: each change
# of a slot adds one to the collection's count of changes and writes the new
# count to the key's cell, so the cell changes whenever the slot does. Calling
# `length()` subscribes to a cell holding the number of slots, and a read of
# every slot to one for the whole collection. Creating a cell costs far more
# than storing a slot, so cells are created on first read, and changing a
# collection writes only the cells that exist. Removing a slot writes its key's
# cell as any change does and then deletes the cell, which by then has no
# subscribers: the write invalidated every reader. A reader that reads the key
# again creates a new cell, and that is the one that fires when the key comes
# back. A cell whose key has no slot stays, because its readers are waiting for
# the key to appear. Since shiny destroys a reactive along with the module it
# was created in, cells are created in the collection's own reactive domain,
# whichever module first reads them.
new_reactives <- function(class) {

  state <- new.env(parent = emptyenv())
  state$keys <- character()
  state$positions <- 0L
  state$changes <- 0L

  structure(
    list(
      slots = new.env(parent = emptyenv()),
      cells = new.env(parent = emptyenv()),
      keys = reactiveVal(
        character(),
        label = paste0("names(", class[[1L]], ")")
      ),
      domain = getDefaultReactiveDomain(),
      state = state
    ),
    class = class
  )
}

raw_keys <- function(x) {
  .subset2(x, "keys")()
}

peek_keys <- function(x) {
  .subset2(x, "state")$keys
}

live_keys <- function(x) {
  check_lifetime(x)
  peek_keys(x)
}

set_keys <- function(x, keys) {

  state <- .subset2(x, "state")

  .subset2(x, "keys")(keys)
  state$keys <- keys

  if (!is.null(state$count)) {
    state$count(length(keys))
  }
}

# Reading a cell fails outside a reactive consumer, and shiny exports no way to
# check for one first. Retrying inside `isolate()` lets `names()` and `length()`
# work at the console, while any other error recurs on the retry. Creating the
# cell can only fail for a destroyed collection, so it happens before the read.
read_anywhere <- function(cell) {
  force(cell)
  tryCatch(cell(), error = function(e) isolate(cell()))
}

slot_cell <- function(x, key) {

  cells <- .subset2(x, "cells")
  cell <- cells[[key]]

  if (is.null(cell)) {
    cell <- new_cell(x, .subset2(x, "state")$changes, cell_label(x, key))
    assign(key, cell, envir = cells)
  }

  cell
}

all_slots_cell <- function(x) {
  state_cell(
    x,
    "all_slots",
    .subset2(x, "state")$changes,
    paste0(class(x)[[1L]], "[]")
  )
}

count_cell <- function(x) {
  state_cell(
    x,
    "count",
    length(peek_keys(x)),
    paste0("length(", class(x)[[1L]], ")")
  )
}

state_cell <- function(x, name, value, label) {

  state <- .subset2(x, "state")

  if (is.null(state[[name]])) {
    state[[name]] <- new_cell(x, value, label)
  }

  state[[name]]
}

new_cell <- function(x, value, label) {

  # Destroying a domain destroys the reactives created in it so far, but not
  # one created afterwards. Without this check, a destroyed collection would
  # get a cell that outlives it.
  check_lifetime(x)

  withReactiveDomain(
    .subset2(x, "domain"),
    reactiveVal(value, label = label)
  )
}

cell_label <- function(x, keys) {

  pos <- is_positional_key(keys)
  keys[pos] <- sub(positional_marker(), "...", keys[pos], fixed = TRUE)

  paste0(class(x)[[1L]], "$", keys)
}

value_label <- function(x, keys) {
  paste0(cell_label(x, keys), "()")
}

get_slot <- function(x, key) {
  slot_cell(x, key)()
  peek_slot(x, key)
}

peek_slot <- function(x, key) {
  .subset2(x, "slots")[[key]]
}

peek_slots <- function(x, keys) {
  mget(keys, envir = .subset2(x, "slots"))
}

get_value <- function(x, key) {

  slot <- get_slot(x, key)

  if (is.null(slot)) {
    return(NULL)
  }

  slot()
}

check_slot <- function(x, value) {

  if (!is.reactive(value)) {
    abort(
      paste(
        "A slot holds a reactive, such as `reactiveVal()` or `reactive()`.",
        "Assign `NULL` with `[<-` to remove a slot, or write a value with",
        "`$<-` or `[[<-`."
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

check_writable <- function(slot) {

  if (!inherits(slot, "reactiveVal")) {
    abort(
      paste(
        "A slot holding a reactive expression can't be written. Bind a",
        "`reactiveVal()` to the slot with `[<-` to replace the expression."
      ),
      "reactives_not_reactive_val"
    )
  }

  invisible(slot)
}

check_collection <- function(x) {

  if (!is_reactives(x)) {
    abort("Expected a `reactives` object.", "reactives_not_collection")
  }

  invisible(x)
}

# Shiny offers no way to ask whether a module was destroyed, but `keys` is
# destroyed along with the collection, and writing to a destroyed
# `reactiveVal()` raises shiny's error. On a live collection, writing back the
# value `keys` holds invalidates nothing and, unlike a read, subscribes no
# caller.
check_lifetime <- function(x) {
  .subset2(x, "keys")(peek_keys(x))
  invisible(x)
}

bind_slot <- function(x, key, value) {

  check_slot(x, value)

  keys <- live_keys(x)

  write_slot(x, key, value)

  if (!key %in% keys) {
    set_keys(x, c(keys, key))
  }

  invisible(x)
}

remove_slot <- function(x, key) {

  keys <- live_keys(x)

  if (key %in% keys) {
    write_slot(x, key, NULL)
    set_keys(x, setdiff(keys, key))
  }

  invisible(x)
}

write_slot <- function(x, key, value) {

  slots <- .subset2(x, "slots")

  if (identical(slots[[key]], value)) {
    return(invisible(x))
  }

  if (is.null(value)) {
    rm(list = key, envir = slots)
  } else {
    assign(key, value, envir = slots)
  }

  state <- .subset2(x, "state")
  state$changes <- state$changes + 1L

  cells <- .subset2(x, "cells")
  cell <- cells[[key]]

  if (!is.null(cell)) {

    cell(state$changes)

    if (is.null(value)) {
      rm(list = key, envir = cells)
    }
  }

  if (!is.null(state$all_slots)) {
    state$all_slots(state$changes)
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

# The `reactiveVal()` for a new slot is created in the collection's domain, as
# a cell is, so that it outlives the module that writes the value.
write_value <- function(x, key, value) {

  check_lifetime(x)
  slot <- peek_slot(x, key)

  if (is.null(slot)) {
    return(bind_slot(x, key, new_cell(x, value, value_label(x, key))))
  }

  check_writable(slot)
  slot(value)

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

read_key <- function(x, i) {

  if (is.character(i)) {
    return(i)
  }

  keys <- raw_keys(x)

  if (i > length(keys)) {
    out_of_bounds(i, length(keys))
  }

  keys[[i]]
}

assign_key <- function(x, i) {

  if (is.character(i)) {
    return(i)
  }

  keys <- live_keys(x)

  if (i == length(keys) + 1L) {
    return(next_positional_key(x))
  }

  if (i > length(keys)) {
    out_of_bounds(i, length(keys))
  }

  keys[[i]]
}

#' @export
`$.reactives` <- function(x, name) {
  get_value(x, name)
}

#' @export
`[[.reactives` <- function(x, i) {
  get_value(x, read_key(x, index2(i)))
}

#' @export
`[.reactives` <- function(x, i) {

  if (missing(i) || length(i) != 1L) {
    abort(
      paste(
        "Subsetting with `[` takes a single name or position. Use",
        "`as.list(x)[i]` for the reactives of several slots."
      ),
      "reactives_unsupported"
    )
  }

  get_slot(x, read_key(x, index2(i)))
}

#' @export
`$<-.reactives` <- function(x, name, value) {
  write_value(x, name, value)
}

#' @export
`[[<-.reactives` <- function(x, i, value) {
  write_value(x, assign_key(x, index2(i)), value)
}

#' @export
`[<-.reactives` <- function(x, i, value) {

  if (missing(i) || length(i) != 1L) {
    abort(
      "Slots are bound or removed one at a time, by a single name or position.",
      "reactives_unsupported"
    )
  }

  assign_slot(x, assign_key(x, index2(i)), value)
}

# A collection is a list underneath, so without this method, base R's
# `names<-` would reach its internal fields.
#' @export
`names<-.reactives` <- function(x, value) {
  abort(
    paste(
      "Slots can't be renamed with `names<-`. Bind the reactive under the new",
      "name with `x[\"new\"] <- x[\"old\"]`, remove the old slot with",
      "`x[\"old\"] <- NULL` and restore the order with `reorder()`."
    ),
    "reactives_unsupported"
  )
}

#' @export
names.reactives <- function(x) {
  display_names(read_anywhere(.subset2(x, "keys")))
}

#' @export
length.reactives <- function(x) {
  read_anywhere(count_cell(x))
}

#' @export
as.list.reactives <- function(x, ...) {

  keys <- raw_keys(x)
  all_slots_cell(x)()

  res <- peek_slots(x, keys)
  names(res) <- display_names(keys)

  res
}

#' @export
format.reactives <- function(x, ...) {

  keys <- live_keys(x)

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

  keys <- live_keys(object)
  slots <- peek_slots(object, keys)

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
