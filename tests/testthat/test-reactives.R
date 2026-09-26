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
      reorder(x, c("c", "a"))
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

      reorder(x, c("b", "a"))
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

test_that("slot_values() returns the value of every slot, in slot order", {

  with_session(
    {
      x <- reactives(a = reactiveVal(1), reactive("u"), b = reactiveVal(NULL))

      expect_identical(slot_values(x), list(a = 1, "u", b = NULL))

      reorder(x, c(3, 1, 2))
      expect_identical(slot_values(x), list(b = NULL, a = 1, "u"))

      expect_identical(slot_values(reactive_vals(1, 2)), list(1, 2))
      expect_identical(slot_values(reactives()), list())
    }
  )
})

test_that("slot_values() re-runs when a slot or its value changes", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      seen <- NULL
      observe(seen <<- slot_values(x))
      session$flushReact()

      x$a(10)
      session$flushReact()
      expect_identical(seen, list(a = 10, b = 2))

      x$c <- reactiveVal(3)
      session$flushReact()
      expect_identical(seen, list(a = 10, b = 2, c = 3))

      x$b <- NULL
      reorder(x, c("c", "a"))
      session$flushReact()
      expect_identical(seen, list(c = 3, a = 10))
    }
  )
})

test_that("slot_values() passes on the error of a computed slot", {

  with_session(
    {
      x <- reactives(a = reactiveVal(1), b = reactive(stop("boom")))
      expect_error(slot_values(x), "boom")
    }
  )
})

test_that("slot_values() needs a collection", {
  expect_error(slot_values(list()), class = "reactives_not_collection")
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

test_that("reorder() takes positions, or names when all are named", {

  with_session(
    {
      u <- reactiveVal("u")
      a <- reactiveVal("a")
      x <- reactives(u, a = a)

      reorder(x, c(2, 1))
      expect_identical(names(x), c("a", ""))
      expect_identical(x[[2]], u)

      expect_error(
        reorder(x, c("a", "")),
        class = "reactives_bad_order"
      )
      expect_error(reorder(x, 1), class = "reactives_bad_order")
      expect_error(reorder(x, c(1, 1)), class = "reactives_bad_order")
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

test_that("printing and inspecting work outside a reactive context", {

  x <- reactive_vals(a = 1, 2)

  expect_identical(format(x), c("<reactive_vals[2]>", "  $a", "  [[2]]"))
  expect_identical(format(reactives()), "<reactives[0]>")
  expect_output(print(x), "<reactive_vals[2]>", fixed = TRUE)
  expect_output(str(x), "<reactive_vals[2]>", fixed = TRUE)

  expect_identical(names(x), c("a", ""))
  expect_identical(length(x), 2L)

  x$b <- reactiveVal(3)
  expect_identical(names(x), c("a", "", "b"))
})

test_that("str() shows the kind of each slot, laid out as for a list", {

  x <- reactives(a = reactiveVal(1), bb = shiny::reactive(2), reactiveVal(3))

  expect_identical(
    capture.output(str(x)),
    c(
      "<reactives[3]>",
      " $ a : reactiveVal",
      " $ bb: reactiveExpr",
      " $   : reactiveVal"
    )
  )

  expect_identical(
    capture.output(str(list(y = reactive_vals(a = 1)))),
    c("List of 1", " $ y:<reactive_vals[1]>", "  ..$ a: reactiveVal")
  )

  expect_identical(capture.output(str(reactives())), "<reactives[0]>")
})

test_that("printing and inspecting don't subscribe or run a computed slot", {

  with_session(
    {
      ran <- FALSE
      x <- reactives(a = reactiveVal(1), b = reactive(ran <<- TRUE))

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          capture.output(print(x), format(x), str(x))
        }
      )
      session$flushReact()

      x$a <- reactiveVal(2)
      x$c <- reactiveVal(3)
      reorder(x, c("c", "b", "a"))
      session$flushReact()

      expect_identical(runs, 1)
      expect_false(ran)
    }
  )
})

test_that("a collection is shared by everything holding it", {

  with_session(
    {
      x <- reactive_vals(a = 1)
      y <- x

      x$b <- reactiveVal(2)
      names(x) <- c("p", "q")
      z <- reorder(x, c("q", "p"))

      expect_identical(names(y), c("q", "p"))
      expect_identical(y$p, x$p)
      expect_identical(z, x)
    }
  )
})

test_that("x[] copies the whole collection", {

  with_session(
    {
      x <- reactive_vals(a = 1, 2)
      y <- x[]

      expect_identical(names(y), c("a", ""))
      expect_identical(y$a, x$a)
      expect_identical(y[[2]], x[[2]])

      y$c <- reactiveVal(3)
      expect_null(x$c)
    }
  )
})

test_that("a slot first read in a module survives the module", {

  with_session(
    {
      x <- reactives()

      moduleServer("child", function(input, output, session) x$a)
      session$destroy("child")

      a <- reactiveVal(1)
      x$a <- a

      expect_identical(x$a, a)
    }
  )
})

test_that("a collection is destroyed along with its module", {

  with_session(
    {
      x <- moduleServer("child", function(input, output, session) reactives())
      x$a <- reactiveVal(1)
      session$destroy("child")

      expect_error(x$a, class = "shiny.destroyed.error")
      expect_error(names(x), class = "shiny.destroyed.error")
    }
  )
})

test_that("[<- binds and removes several slots by name", {

  with_session(
    {
      x <- reactives()
      a <- reactiveVal(1)
      b <- reactive(2)

      x[c("a", "b")] <- list(a, b)

      expect_identical(names(x), c("a", "b"))
      expect_identical(x$b, b)

      x[c("a", "zzz")] <- NULL
      expect_identical(names(x), "b")

      c1 <- reactiveVal(3)
      x[c("b", "c")] <- c1

      expect_identical(x$b, c1)
      expect_identical(x$c, c1)

      x[c("b", "c")] <- list(NULL, reactiveVal(4))
      expect_identical(names(x), "c")
    }
  )
})

test_that("[<- works by position and on every slot", {

  with_session(
    {
      u1 <- reactiveVal("u1")
      u2 <- reactiveVal("u2")
      x <- reactives(u1, a = reactiveVal("a"))

      x[1] <- list(u2)
      expect_identical(x[[1]], u2)

      x[] <- list(u1, u2)

      expect_identical(names(x), c("", "a"))
      expect_identical(x[[1]], u1)
      expect_identical(x$a, u2)

      expect_error(x[3] <- list(u1), class = "reactives_out_of_bounds")

      x[] <- NULL
      expect_identical(length(x), 0L)
    }
  )
})

test_that("[<- checks every value before changing anything", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      expect_error(
        x[c("a", "b")] <- list(NULL, 1),
        class = "reactives_not_reactive"
      )
      expect_identical(names(x), "a")

      expect_error(
        x[c("a", "b")] <- list(reactiveVal(1), reactiveVal(2), reactiveVal(3)),
        class = "reactives_bad_value"
      )
      expect_identical(names(x), "a")
    }
  )
})

test_that("reactive_vals() only accepts reactiveVal slots", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      expect_s3_class(x, "reactive_vals")
      expect_s3_class(x, "reactives")
      expect_s3_class(x["a"], "reactive_vals")

      expect_error(x$b <- reactive(2), class = "reactives_not_reactive_val")

      x$b <- reactiveVal(2)
      expect_identical(x$b(), 2)

      y <- reactives(a = reactiveVal(1), b = reactive(2))
      expect_false(inherits(y, "reactive_vals"))
    }
  )
})

test_that("is_reactives() is TRUE for either kind of collection only", {

  expect_true(is_reactives(reactives()))
  expect_true(is_reactives(reactive_vals(a = 1)))

  expect_false(is_reactives(list()))
  expect_false(is_reactives(shiny::reactiveValues()))
  expect_false(is_reactives(reactiveVal(1)))
})

test_that("subscripts follow vctrs' rules rather than base R's", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2, 3)

      expect_error(x[1.5], class = "reactives_bad_index")
      expect_error(x[Inf], class = "reactives_bad_index")
      expect_error(x[c(1, NA)], class = "reactives_bad_index")
      expect_error(x[c(-1, 2)], class = "reactives_bad_index")
      expect_error(x[-5], class = "reactives_out_of_bounds")
      expect_error(x[c(TRUE, FALSE)], class = "reactives_bad_index")
      expect_error(x[""], class = "reactives_bad_index")
      expect_error(x[list(1)], class = "reactives_bad_index")

      expect_identical(names(x[TRUE]), c("a", "b", ""))
      expect_identical(length(x[NULL]), 0L)
      expect_identical(names(x[c(0, 2)]), "b")
      expect_identical(names(x[factor("b")]), "b")

      expect_error(x[[0]], class = "reactives_bad_index")
      expect_error(x[[-1]], class = "reactives_bad_index")
      expect_error(x[[TRUE]], class = "reactives_bad_index")
      expect_error(x[[NA_integer_]], class = "reactives_bad_index")
      expect_error(x[[""]], class = "reactives_bad_index")

      expect_error(
        x[c(TRUE, FALSE)] <- list(reactiveVal(1)),
        class = "reactives_bad_index"
      )
      expect_error(
        x[1:3] <- list(reactiveVal(1), reactiveVal(2)),
        class = "reactives_bad_value"
      )
    }
  )
})
