check_names <- function(i) {

  if (!all(nzchar(i))) {
    abort("A subscript can't contain the empty string.", "reactives_bad_index")
  }

  i
}

subscript_names <- function(i) {

  if (!is.character(i) && !is.factor(i)) {
    abort(
      "Slot names must be given as a character vector.",
      "reactives_bad_index"
    )
  }

  i <- as.character(i)

  if (anyNA(i)) {
    abort("A slot name can't be missing.", "reactives_bad_index")
  }

  check_names(i)
}

index2 <- function(i) {

  if (is.factor(i) || is.character(i)) {
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
