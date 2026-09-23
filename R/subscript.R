# Subscripts follow vctrs' rules rather than base R's: positions are never
# truncated, a logical subscript only recycles from size 1, and out-of-range
# negative positions are errors rather than ignored. Missing values are errors
# too, since a collection can't hold a missing slot.
subscript_positions <- function(i, n, names) {

  if (is.null(i)) {
    return(integer())
  }

  if (is.factor(i)) {
    i <- as.character(i)
  }

  if (anyNA(i)) {
    abort("A subscript can't contain missing values.", "reactives_bad_index")
  }

  if (is.logical(i)) {
    return(logical_positions(i, n))
  }

  if (is.numeric(i)) {
    return(numeric_positions(i, n))
  }

  if (is.character(i)) {
    return(name_positions(i, names))
  }

  abort(
    "A subscript must be logical, numeric or character.",
    "reactives_bad_index"
  )
}

logical_positions <- function(i, n) {

  if (!length(i) %in% c(1L, n)) {
    abort(
      sprintf(
        "A logical subscript must be size 1 or %d, not %d.",
        n,
        length(i)
      ),
      "reactives_bad_index"
    )
  }

  which(rep_len(i, n))
}

numeric_positions <- function(i, n) {

  if (!all(is.finite(i) & i == trunc(i))) {
    abort(
      "A numeric subscript must hold whole numbers.",
      "reactives_bad_index"
    )
  }

  i <- i[i != 0]

  if (any(i < 0) && any(i > 0)) {
    abort(
      "Negative and positive positions can't be mixed.",
      "reactives_bad_index"
    )
  }

  beyond <- abs(i) > n

  if (any(beyond)) {
    out_of_bounds(abs(i[beyond][[1L]]), n)
  }

  if (length(i) && i[[1L]] < 0) {
    return(setdiff(seq_len(n), -i))
  }

  as.integer(i)
}

name_positions <- function(i, names) {

  check_names(i)

  pos <- match(i, names)
  unknown <- i[is.na(pos)]

  if (length(unknown)) {
    abort(
      paste0("Unknown slot names: ", paste(unknown, collapse = ", "), "."),
      "reactives_unknown_name"
    )
  }

  pos
}

check_names <- function(i) {

  if (!all(nzchar(i))) {
    abort("A subscript can't contain the empty string.", "reactives_bad_index")
  }

  i
}

select_keys <- function(keys, i) {
  keys[subscript_positions(i, length(keys), display_names(keys))]
}

target_keys <- function(keys, i) {

  if (is.factor(i)) {
    i <- as.character(i)
  }

  if (!is.character(i)) {
    return(select_keys(keys, i))
  }

  if (anyNA(i)) {
    abort("A subscript can't contain missing values.", "reactives_bad_index")
  }

  check_names(i)
}

index2 <- function(i) {

  if (is.factor(i)) {
    i <- as.character(i)
  }

  if (length(i) != 1L) {
    abort(
      sprintf(
        "A slot is indexed by a single name or position, not %d values.",
        length(i)
      ),
      "reactives_bad_index"
    )
  }

  if (is.character(i)) {

    if (is.na(i)) {
      abort("A slot name can't be missing.", "reactives_bad_index")
    }

    return(check_names(i))
  }

  if (!is.numeric(i)) {
    abort("A slot is indexed by a name or a position.", "reactives_bad_index")
  }

  if (is.na(i) || !is.finite(i) || i != trunc(i) || i < 1) {
    abort(
      sprintf("A position must be a positive whole number, not %s.", i),
      "reactives_bad_index"
    )
  }

  i
}

out_of_bounds <- function(i, n) {
  abort(
    sprintf("Position %d is beyond the %d slot(s).", i, n),
    "reactives_out_of_bounds"
  )
}

recycle_values <- function(value, n) {

  if (is.null(value)) {
    return(rep(list(NULL), n))
  }

  if (is.reactive(value)) {
    return(rep(list(value), n))
  }

  if (!is.list(value)) {
    abort(
      "Assign `NULL`, a single reactive, or a list of reactives.",
      "reactives_bad_value"
    )
  }

  if (!length(value) %in% c(1L, n)) {
    abort(
      sprintf(
        "Can't assign %d values to %d slots; supply 1 or %d.",
        length(value),
        n,
        n
      ),
      "reactives_bad_value"
    )
  }

  rep_len(value, n)
}
