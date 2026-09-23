test_that("reading a slot returns its reactive, or NULL for no slot", {

  with_session(
    {
      a <- reactiveVal(1)
      x <- reactives(a = a)

      expect_identical(x$a, a)
      expect_identical(x[["a"]], a)
      expect_identical(x[[1]], a)
      expect_identical(x$a(), 1)

      expect_null(x$b)
      expect_null(x[["b"]])
    }
  )
})

test_that("a stored NULL is distinct from a missing slot", {

  with_session(
    {
      x <- reactive_vals(a = NULL)

      expect_true(is.reactive(x$a))
      expect_null(x$a())
      expect_identical(names(x), "a")

      expect_null(x$b)
    }
  )
})

test_that("assigning NULL removes a slot, as for a list", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      x$a <- NULL

      expect_null(x$a)
      expect_identical(names(x), "b")
      expect_identical(length(x), 1L)

      x[["b"]] <- NULL
      expect_identical(length(x), 0L)

      x$zzz <- NULL
      expect_identical(length(x), 0L)
    }
  )
})

test_that("only a reactive can be bound to a slot", {

  with_session(
    {
      x <- reactives()

      expect_error(x$a <- 1, class = "reactives_not_reactive")
      expect_error(x[["a"]] <- function() 1, class = "reactives_not_reactive")
      expect_error(reactives(a = 1), class = "reactives_not_reactive")

      expect_identical(length(x), 0L)
    }
  )
})

test_that("the constructor drops NULL entries and needs distinct names", {

  with_session(
    {
      x <- reactives(a = NULL, b = reactiveVal(1))
      expect_identical(names(x), "b")

      expect_error(
        reactives(a = reactiveVal(1), a = reactiveVal(2)),
        class = "reactives_duplicate_name"
      )
    }
  )
})

test_that("a reader of one slot isn't re-run when another slot changes", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          x$a
        }
      )
      session$flushReact()

      x$b <- reactiveVal(3)
      x$c <- reactiveVal(4)
      x$b <- NULL
      reorder_reactives(x, c("c", "a"))
      session$flushReact()

      expect_identical(runs, 1)
    }
  )
})

test_that("a reader of a missing slot re-runs when the slot is added", {

  with_session(
    {
      x <- reactives()

      seen <- "unset"
      observe(seen <<- x$a)
      session$flushReact()

      expect_null(seen)

      a <- reactiveVal(1)
      x$a <- a
      session$flushReact()

      expect_identical(seen, a)
    }
  )
})

test_that("a reader re-runs on removal and again when the slot is re-added", {

  with_session(
    {
      a <- reactiveVal(1)
      x <- reactives(a = a)

      runs <- 0
      seen <- NULL
      observe(
        {
          runs <<- runs + 1
          seen <<- x$a
        }
      )
      session$flushReact()

      x$a <- NULL
      session$flushReact()

      expect_identical(runs, 2)
      expect_null(seen)

      a2 <- reactiveVal(2)
      x$a <- a2
      session$flushReact()

      expect_identical(runs, 3)
      expect_identical(seen, a2)
    }
  )
})

test_that("replacing a slot's reactive re-runs readers of its value", {

  with_session(
    {
      x <- reactives(a = reactive("first"))

      seen <- NULL
      observe(seen <<- x$a())
      session$flushReact()

      expect_identical(seen, "first")

      x$a <- reactive("second")
      session$flushReact()

      expect_identical(seen, "second")
    }
  )
})

test_that("writing through a stored slot re-runs readers of its value", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      seen <- NULL
      observe(seen <<- x$a())
      session$flushReact()

      x$a(5)
      session$flushReact()

      expect_identical(seen, 5)
    }
  )
})

test_that("names() and length() don't re-run on slot changes", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          names(x)
          length(x)
        }
      )
      session$flushReact()

      x$a(2)
      x$a <- reactiveVal(3)
      session$flushReact()

      expect_identical(runs, 1)

      x$b <- reactiveVal(4)
      session$flushReact()

      expect_identical(runs, 2)
    }
  )
})

test_that("reordering re-runs readers of names() but not of single slots", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      name_runs <- 0
      slot_runs <- 0

      observe(
        {
          name_runs <<- name_runs + 1
          names(x)
        }
      )

      observe(
        {
          slot_runs <<- slot_runs + 1
          x$a
        }
      )

      session$flushReact()

      reorder_reactives(x, c("b", "a"))
      session$flushReact()

      expect_identical(name_runs, 2)
      expect_identical(slot_runs, 1)
      expect_identical(names(x), c("b", "a"))
    }
  )
})

test_that("as.list() returns the slots and re-runs on any change", {

  with_session(
    {
      a <- reactiveVal(1)
      x <- reactives(a = a)

      seen <- NULL
      observe(seen <<- as.list(x))
      session$flushReact()

      expect_identical(seen, list(a = a))

      b <- reactive(2)
      x$b <- b
      session$flushReact()

      expect_identical(seen, list(a = a, b = b))

      a2 <- reactiveVal(3)
      x$a <- a2
      session$flushReact()

      expect_identical(seen, list(a = a2, b = b))
    }
  )
})

test_that("unnamed slots are read and removed by position", {

  with_session(
    {
      a <- reactiveVal("a")
      u1 <- reactiveVal("u1")
      u2 <- reactiveVal("u2")

      x <- reactives(u1, a = a, u2)

      expect_identical(names(x), c("", "a", ""))
      expect_identical(x[[1]], u1)
      expect_identical(x[[3]], u2)
      expect_identical(as.list(x), list(u1, a = a, u2))

      x[[1]] <- NULL
      expect_identical(names(x), c("a", ""))
      expect_identical(x[[2]], u2)

      expect_null(names(reactives(u1, u2)))
    }
  )
})

test_that("assigning one past the end appends an unnamed slot", {

  with_session(
    {
      x <- reactive_vals(a = 1)
      u <- reactiveVal("u")

      x[[length(x) + 1]] <- u

      expect_identical(names(x), c("a", ""))
      expect_identical(x[[2]], u)

      x[[length(x) + 1]] <- NULL
      expect_identical(length(x), 2L)
    }
  )
})

test_that("positions out of range and malformed indices are errors", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      expect_error(x[[2]], class = "reactives_out_of_bounds")
      expect_error(x[[3]] <- reactiveVal(1), class = "reactives_out_of_bounds")
      expect_error(x[[c("a", "b")]], class = "reactives_bad_index")
      expect_error(x[[1.5]], class = "reactives_bad_index")
    }
  )
})

test_that("names<- renames slots in place", {

  with_session(
    {
      a <- reactiveVal("a")
      b <- reactiveVal("b")
      x <- reactives(a = a, b = b)

      names(x) <- c("b", "a")

      expect_identical(x$b, a)
      expect_identical(x$a, b)

      names(x) <- c("", "c")

      expect_identical(names(x), c("", "c"))
      expect_identical(x[[1]], a)
      expect_identical(x$c, b)
      expect_null(x$a)

      names(x) <- NULL
      expect_null(names(x))

      expect_error(names(x) <- c("d", "d"), class = "reactives_duplicate_name")
      expect_error(names(x) <- "d", class = "reactives_bad_names")
    }
  )
})

test_that("renaming re-runs readers of the old and the new name", {

  with_session(
    {
      a <- reactiveVal("a")
      x <- reactives(a = a)

      seen_a <- "unset"
      seen_z <- "unset"
      observe(seen_a <<- x$a)
      observe(seen_z <<- x$z)
      session$flushReact()

      names(x) <- "z"
      session$flushReact()

      expect_null(seen_a)
      expect_identical(seen_z, a)
    }
  )
})

test_that("reorder_reactives() takes positions, or names when all are named", {

  with_session(
    {
      u <- reactiveVal("u")
      a <- reactiveVal("a")
      x <- reactives(u, a = a)

      reorder_reactives(x, c(2, 1))
      expect_identical(names(x), c("a", ""))
      expect_identical(x[[2]], u)

      expect_error(
        reorder_reactives(x, c("a", "")),
        class = "reactives_bad_order"
      )
      expect_error(reorder_reactives(x, 1), class = "reactives_bad_order")
      expect_error(reorder_reactives(x, c(1, 1)), class = "reactives_bad_order")
      expect_error(reorder_reactives(list(), 1), class = "reactives_bad_object")
    }
  )
})

test_that("[ returns a new collection holding the same reactives", {

  with_session(
    {
      a <- reactiveVal("a")
      b <- reactiveVal("b")
      u <- reactiveVal("u")
      x <- reactives(a = a, b = b, u)

      y <- x[c("b", "a")]

      expect_identical(names(y), c("b", "a"))
      expect_identical(y$a, a)

      z <- x[c(3, 1)]
      expect_identical(names(z), c("", "a"))
      expect_identical(z[[1]], u)

      expect_identical(names(x[-1]), c("b", ""))
      expect_identical(names(x[c(TRUE, FALSE, TRUE)]), c("a", ""))

      y$c <- reactiveVal("c")
      expect_null(x$c)

      expect_error(x["zzz"], class = "reactives_unknown_name")
      expect_error(x[4], class = "reactives_out_of_bounds")
    }
  )
})

test_that("format() and print() work outside a reactive context", {

  x <- reactive_vals(a = 1, 2)

  expect_identical(format(x), c("<reactives[2]>", "  $a", "  [[2]]"))
  expect_identical(format(reactives()), "<reactives[0]>")
  expect_output(print(x), "<reactives[2]>", fixed = TRUE)
})
