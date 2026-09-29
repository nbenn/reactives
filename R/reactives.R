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
#' the result to get its value, as in `x$a()`, or get the value of every slot
#' at once with `slot_values()`, as [shiny::reactiveValuesToList()] does for a
#' [shiny::reactiveValues()] object. Because a slot holds a reactive rather
#' than a value, a slot whose [shiny::reactiveVal()] stores `NULL` is distinct
#' from a missing slot.
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
#' A copy made with `[` is shallow: its slots hold the same reactives as the
#' original, so after `y <- x["a"]`, calling `y$a(10)` also sets the value of
#' `x$a()`. A deep copy, made with `copy(x, deep = TRUE)`, gives each
#' [shiny::reactiveVal()] slot a new [shiny::reactiveVal()] holding the slot's
#' current value, so writing through a slot of the copy leaves the original
#' unchanged. Any other slot, such as a [shiny::reactive()], stays shared,
#' since it can't be copied and can't be written through either. A computed
#' slot that reads slots of the original keeps reading the original, not the
#' copy. By default, `copy()` makes the same shallow copy as `x[]`.
#'
#' @section Lifetime:
#' A collection belongs to the session or module it was created in and is
#' destroyed along with it, as a [shiny::reactiveVal()] is. Reading or changing
#' it from another module doesn't tie it to that module, so destroying the
#' module leaves the collection intact. A reactive bound to a slot, however,
#' still belongs to the module it was created in. A copy belongs to the session
#' or module it is made in, and so does each new [shiny::reactiveVal()] of a
#' deep copy.
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
#' Reading a slot by name makes the caller depend on that slot alone. The
#' caller re-runs when the slot is bound, replaced or removed, but not when
#' other slots change. This also holds for a slot that does not exist yet, so a
#' reader re-runs once the slot is added. Subsetting with `[` by name depends
#' on the named slots in the same way.
#'
#' Reading a slot by position, as in `x[[1]]`, depends on the reactive at that
#' position alone. The caller re-runs when the slot there is replaced or
#' removed or another slot moves there, but not when a slot is renamed or when
#' only slots after it change. Reading past the last slot fails, and the
#' caller re-runs when the number of slots changes.
#'
#' The `length()` method depends on the number of slots alone, and `names()`
#' on which slots exist, their names and their order. Subsetting by position
#' depends on the selected slots and, since the subset carries their names, on
#' what `names()` depends on. The `as.list()` method depends on that and on
#' every slot, and `slot_values()` also on every slot's value. Copying, with
#' `x[]` or `copy()`, has the same dependencies as `as.list()`. A deep copy
#' reads the values it copies without depending on them, and it runs no
#' computed slot. Reordering re-runs readers of `names()`, `as.list()`,
#' `slot_values()` and of subsets by position, and readers of a position that
#' another slot moves into, but not readers of `length()` or of slots by name.
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
#' collection, the one on the slot at position 1 as `reactives[[1]]`, the one
#' on which slots exist and in what order as `names(reactives)`, and the one on
#' their number as `length(reactives)`. Unnamed slots selected with `[` show as
#' `reactives$...1`, `reactives$...2` and so on, numbered in the order they
#' were added rather than by position, so a label survives reordering. A read
#' of every slot at once, such as `as.list()`, shows a single dependency on
#' `reactives[]` instead of one per slot. The entry for a slot or a position
#' only appears once it has been read, and the one for the number of slots
#' once `length()` has been called.
#'
#' @param ... For `reactives()` and `reactive_vals()`, the slots to hold, named
#'   or unnamed: reactives, with `NULL` entries dropped, for `reactives()`, and
#'   any values, including `NULL`, for `reactive_vals()`. Ignored by
#'   `reorder()`.
#'
#' @return A `reactives` object, or for `reactive_vals()` a `reactive_vals`
#'   object, which is also a `reactives` object. A copy or a snapshot has the
#'   class of `x`. The `reorder()` method returns `x`, invisibly,
#'   `slot_values()` returns a list of the slots' values, in slot order and
#'   named as by `as.list()`, and `is_reactives()` returns `TRUE` or `FALSE`.
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
#' shiny::isolate(slot_values(y))
#' is_reactives(y)
#'
#' # A deep copy holds values of its own
#' w <- shiny::isolate(copy(y, deep = TRUE))
#' shiny::isolate(w$n(2))
#' shiny::isolate(c(y$n(), w$n()))
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
#' shiny::isolate(snap$n())
#'
#' @export
reactives <- function(...) {
  build_reactives(list(...), "reactives")
}

#' @rdname reactives
#' @export
reactive_vals <- function(...) {
  build_reactives(
    lapply(list(...), reactiveVal),
    c("reactive_vals", "reactives")
  )
}

#' @param x A `reactives` object, or for `is_reactives()`, any object.
#' @param order The new order, listing every slot exactly once: by position, or
#'   by name when all slots are named.
#'
#' @rdname reactives
#' @export
reorder.reactives <- function(x, order, ...) {

  keys <- live_keys(x)
  new_keys <- if (is.numeric(order)) keys[order] else as.character(order)

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

#' @rdname reactives
#' @export
slot_values <- function(x) {
  check_collection(x)
  lapply(as.list(x), do.call, list())
}

#' @param deep Whether to give each [shiny::reactiveVal()] slot of the copy a
#'   new [shiny::reactiveVal()] holding the slot's current value, rather than
#'   the one in `x`.
#'
#' @rdname reactives
#' @export
copy <- function(x, deep = FALSE) {

  check_collection(x)

  if (!isTRUE(deep) && !isFALSE(deep)) {
    abort("Expected `deep` to be `TRUE` or `FALSE`.", "reactives_bad_flag")
  }

  slots <- as.list(x)

  if (deep) {

    stored <- vapply(slots, inherits, logical(1L), "reactiveVal")
    values <- isolate(lapply(slots[stored], do.call, list()))
    slots[stored] <- lapply(values, reactiveVal)
  }

  build_reactives(slots, class(x))
}

build_reactives <- function(slots, class) {

  slots <- slots[!vapply(slots, is.null, logical(1L))]

  keys <- names(slots)

  if (is.null(keys)) {
    keys <- character(length(slots))
  }

  named <- nzchar(keys)

  if (anyDuplicated(keys[named])) {
    abort("Each slot needs a distinct name.", "reactives_duplicate_name")
  }

  res <- new_reactives(class)

  for (slot in slots) {
    check_slot(res, slot)
  }

  for (i in which(!named)) {
    keys[[i]] <- next_positional_key(res)
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
# count to the key's cell, so the cell changes whenever the slot does. Reading a
# position subscribes to a cell holding the reactive at that position, which
# renaming leaves as it is, `length()` to one holding the number of slots, and a
# read of every slot to one for the whole collection. Creating a cell costs far
# more than storing a slot, so cells are created on first read, and changing a
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
  state$position_cells <- list()
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
  old <- state$keys

  .subset2(x, "keys")(keys)
  state$keys <- keys

  if (!is.null(state$count)) {
    state$count(length(keys))
  }

  positions <- seq_len(max(length(old), length(keys)))
  same <- old[positions] == keys[positions]

  refresh_positions(x, which(is.na(same) | !same))
}

# Only a position whose key changed, or whose slot was replaced, can hold a new
# reactive. Writing a cell the reactive it already holds, as after a rename,
# invalidates nothing.
refresh_positions <- function(x, positions) {

  cells <- .subset2(x, "state")$position_cells

  for (i in positions[positions <= length(cells)]) {
    if (!is.null(cells[[i]])) {
      cells[[i]](slot_at(x, i))
    }
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

position_cell <- function(x, i) {

  state <- .subset2(x, "state")
  cells <- state$position_cells
  cell <- if (i <= length(cells)) cells[[i]]

  if (is.null(cell)) {

    # A `reactiveVal()` holds on to its initial value for as long as it exists,
    # so a position cell starts empty and is set afterwards. Starting it at the
    # slot would keep that slot alive once it is replaced.
    cell <- new_cell(x, NULL, sprintf("%s[[%d]]", class(x)[[1L]], i))
    cell(slot_at(x, i))

    state$position_cells[[i]] <- cell
  }

  cell
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

cell_label <- function(x, key) {

  if (is_positional_key(key)) {
    key <- sub(positional_marker(), "...", key, fixed = TRUE)
  }

  paste0(class(x)[[1L]], "$", key)
}

get_slot <- function(x, key) {
  slot_cell(x, key)()
  .subset2(x, "slots")[[key]]
}

peek_slots <- function(x, keys) {
  mget(keys, envir = .subset2(x, "slots"))
}

slot_at <- function(x, i) {

  keys <- peek_keys(x)

  if (i <= length(keys)) {
    .subset2(x, "slots")[[keys[[i]]]]
  }
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
  at <- match(key, keys)

  write_slot(x, key, value)
  refresh_all_slots(x)

  if (is.na(at)) {
    set_keys(x, c(keys, key))
  } else {
    refresh_positions(x, at)
  }

  invisible(x)
}

remove_slot <- function(x, key) {

  keys <- live_keys(x)

  if (key %in% keys) {
    write_slot(x, key, NULL)
    refresh_all_slots(x)
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

  invisible(x)
}

# One write of the cell invalidates every reader of the whole collection, so a
# method that changes slots writes it once, after its last `write_slot()`,
# rather than once per slot. A call that changes no slot leaves `changes` as it
# was, and writing a cell the value it already holds invalidates nothing.
refresh_all_slots <- function(x) {

  state <- .subset2(x, "state")

  if (!is.null(state$all_slots)) {
    state$all_slots(state$changes)
  }
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

  if (i > length(peek_keys(x))) {
    out_of_bounds(i, length(x))
  }

  position_cell(x, i)()
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

  keys <- live_keys(x)

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

  if (missing(i)) {
    slots <- as.list(x)
  } else if (is_name_subscript(i)) {
    slots <- named_slots(x, subscript_names(i))
  } else {
    slots <- positioned_slots(x, i)
  }

  build_reactives(slots, class(x))
}

named_slots <- function(x, i) {

  slots <- lapply(i, get_slot, x = x)

  unknown <- vapply(slots, is.null, logical(1L)) | is_positional_key(i)

  if (any(unknown)) {
    abort(
      paste0("Unknown slot names: ", paste(i[unknown], collapse = ", "), "."),
      "reactives_unknown_name"
    )
  }

  names(slots) <- i

  slots
}

positioned_slots <- function(x, i) {

  keys <- raw_keys(x)
  sel <- select_keys(keys, i)

  if (all(keys %in% sel)) {
    all_slots_cell(x)()
    slots <- peek_slots(x, sel)
  } else {
    slots <- lapply(sel, get_slot, x = x)
  }

  names(slots) <- replace(sel, is_positional_key(sel), "")

  slots
}

#' @export
`[<-.reactives` <- function(x, i, value) {

  keys <- live_keys(x)
  targets <- if (missing(i)) keys else target_keys(keys, i)
  values <- recycle_values(value, length(targets))

  # Check every value before binding any, so that a bad element leaves the
  # collection unchanged.
  for (slot in values[!vapply(values, is.null, logical(1L))]) {
    check_slot(x, slot)
  }

  for (j in seq_along(targets)) {

    key <- targets[[j]]
    write_slot(x, key, values[[j]])

    if (is.null(values[[j]])) {
      keys <- keys[keys != key]
    } else if (!key %in% keys) {
      keys <- c(keys, key)
    }
  }

  refresh_all_slots(x)
  set_keys(x, keys)

  # Rebinding a slot in place leaves its key where it was, so `set_keys()`
  # doesn't refresh its position.
  refresh_positions(x, which(keys %in% targets))

  x
}

#' @export
names.reactives <- function(x) {
  display_names(read_anywhere(.subset2(x, "keys")))
}

#' @export
`names<-.reactives` <- function(x, value) {

  keys <- live_keys(x)

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

  # Read every moving slot before writing any, so that swapping two names does
  # not overwrite a slot before it has moved.
  slots <- peek_slots(x, keys[moved])

  for (key in setdiff(keys, new_keys)) {
    write_slot(x, key, NULL)
  }

  for (j in seq_along(slots)) {
    write_slot(x, new_keys[moved][[j]], slots[[j]])
  }

  refresh_all_slots(x)
  set_keys(x, new_keys)

  x
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
