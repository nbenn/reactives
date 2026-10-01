test_that("reading with [ returns a slot's reactive, or NULL for no slot", {

  with_session(
    {
      a <- reactiveVal(1)
      x <- reactives(a = a)

      expect_identical(x["a"], a)
      expect_identical(x[1], a)

      expect_null(x["b"])
    }
  )
})

test_that("reading with $ or [[ returns a slot's value, or NULL for no slot", {

  with_session(
    {
      x <- reactives(a = reactiveVal(1), b = reactive(2))

      expect_identical(x$a, 1)
      expect_identical(x[["a"]], 1)
      expect_identical(x[[2]], 2)

      expect_null(x$c)
      expect_null(x[["c"]])
    }
  )
})

test_that("a stored NULL is distinct from a missing slot", {

  with_session(
    {
      x <- reactive_vals(a = NULL)

      expect_true(is.reactive(x["a"]))
      expect_null(x$a)
      expect_identical(names(x), "a")

      expect_null(x["b"])
    }
  )
})

test_that("assigning NULL with [<- removes a slot, as for a list", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      x["a"] <- NULL

      expect_null(x["a"])
      expect_identical(names(x), "b")
      expect_identical(length(x), 1L)

      x[1] <- NULL
      expect_identical(length(x), 0L)

      x["zzz"] <- NULL
      expect_identical(length(x), 0L)
    }
  )
})

test_that("only a reactive can be bound to a slot", {

  with_session(
    {
      x <- reactives()

      expect_error(x["a"] <- 1, class = "reactives_not_reactive")
      expect_error(x[1] <- function() 1, class = "reactives_not_reactive")
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
          x["a"]
        }
      )
      session$flushReact()

      x["b"] <- reactiveVal(3)
      x["c"] <- reactiveVal(4)
      x["b"] <- NULL
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
      observe(seen <<- x["a"])
      session$flushReact()

      expect_null(seen)

      a <- reactiveVal(1)
      x["a"] <- a
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
          seen <<- x["a"]
        }
      )
      session$flushReact()

      x["a"] <- NULL
      session$flushReact()

      expect_identical(runs, 2)
      expect_null(seen)

      a2 <- reactiveVal(2)
      x["a"] <- a2
      session$flushReact()

      expect_identical(runs, 3)
      expect_identical(seen, a2)
    }
  )
})

test_that("a reader of a value re-runs when its slot or the value changes", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      runs <- 0
      seen <- NULL
      observe(
        {
          runs <<- runs + 1
          seen <<- x[["a"]]
        }
      )
      session$flushReact()

      x$b <- 3
      x$c <- 4
      reorder(x, c("c", "b", "a"))
      session$flushReact()

      expect_identical(runs, 1)

      x$a <- 5
      session$flushReact()

      expect_identical(runs, 2)
      expect_identical(seen, 5)

      x["a"] <- reactiveVal(6)
      session$flushReact()

      expect_identical(runs, 3)
      expect_identical(seen, 6)

      x["a"] <- NULL
      session$flushReact()

      expect_identical(runs, 4)
      expect_null(seen)
    }
  )
})

test_that("replacing a slot's reactive re-runs readers of its value", {

  with_session(
    {
      x <- reactives(a = reactive("first"))

      seen <- NULL
      observe(seen <<- x$a)
      session$flushReact()

      expect_identical(seen, "first")

      x["a"] <- reactive("second")
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
      observe(seen <<- x$a)
      session$flushReact()

      x$a <- 5
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

      x$a <- 2
      x["a"] <- reactiveVal(3)
      session$flushReact()

      expect_identical(runs, 1)

      x["b"] <- reactiveVal(4)
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
          x["a"]
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
      x["b"] <- b
      session$flushReact()

      expect_identical(seen, list(a = a, b = b))

      a2 <- reactiveVal(3)
      x["a"] <- a2
      session$flushReact()

      expect_identical(seen, list(a = a2, b = b))
    }
  )
})

test_that("a call that changes no slot re-runs no reader of every slot", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          as.list(x)
        }
      )
      session$flushReact()

      x["a"] <- x["a"]
      session$flushReact()

      expect_identical(runs, 1)
    }
  )
})

test_that("as_values() returns the value of every slot, in slot order", {

  with_session(
    {
      x <- reactives(a = reactiveVal(1), reactive("u"), b = reactiveVal(NULL))

      expect_identical(as_values(x), list(a = 1, "u", b = NULL))

      reorder(x, c(3, 1, 2))
      expect_identical(as_values(x), list(b = NULL, a = 1, "u"))

      expect_identical(as_values(reactive_vals(1, 2)), list(1, 2))
      expect_identical(as_values(reactives()), list())
    }
  )
})

test_that("as_values() re-runs when a slot or its value changes", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      seen <- NULL
      observe(seen <<- as_values(x))
      session$flushReact()

      x$a <- 10
      session$flushReact()
      expect_identical(seen, list(a = 10, b = 2))

      x["c"] <- reactiveVal(3)
      session$flushReact()
      expect_identical(seen, list(a = 10, b = 2, c = 3))

      x["b"] <- NULL
      reorder(x, c("c", "a"))
      session$flushReact()
      expect_identical(seen, list(c = 3, a = 10))
    }
  )
})

test_that("as_values() passes on the error of a computed slot", {

  with_session(
    {
      x <- reactives(a = reactiveVal(1), b = reactive(stop("boom")))
      expect_error(as_values(x), "boom")
    }
  )
})

test_that("as_values() needs a collection", {
  expect_error(as_values(list()), class = "reactives_not_collection")
})

test_that("has_key() tests for each slot by name", {

  with_session(
    {
      x <- reactives(a = reactiveVal(1), reactiveVal(2), b = reactiveVal(NULL))

      expect_identical(has_key(x, c("a", "b", "c")), c(TRUE, TRUE, FALSE))
      expect_identical(has_key(x, c(p = "b", q = "c")), c(TRUE, FALSE))
      expect_identical(has_key(x, factor(c("c", "a"))), c(FALSE, TRUE))
      expect_identical(has_key(x, character()), logical())
    }
  )
})

test_that("has_key() depends on the slots it tests for alone", {

  with_session(
    {
      x <- reactive_vals(b = 1, c = 2)

      runs <- 0
      seen <- NULL
      observe(
        {
          runs <<- runs + 1
          seen <<- has_key(x, c("a", "b"))
        }
      )
      session$flushReact()

      expect_identical(seen, c(FALSE, TRUE))

      x["d"] <- reactiveVal(3)
      reorder(x, c("d", "c", "b"))
      x$b <- 4
      session$flushReact()

      expect_identical(runs, 1)

      x["b"] <- NULL
      session$flushReact()

      expect_identical(runs, 2)
      expect_identical(seen, c(FALSE, FALSE))

      x["a"] <- reactiveVal(5)
      session$flushReact()

      expect_identical(runs, 3)
      expect_identical(seen, c(TRUE, FALSE))
    }
  )
})

test_that("has_key() needs a collection and slot names", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      expect_error(has_key(list(), "a"), class = "reactives_not_collection")
      expect_error(has_key(x, 1), class = "reactives_bad_index")
      expect_error(has_key(x, NULL), class = "reactives_bad_index")
      expect_error(has_key(x, c("a", NA)), class = "reactives_bad_index")
      expect_error(has_key(x, c("a", "")), class = "reactives_bad_index")
    }
  )
})

test_that("writing a value goes through the slot's reactiveVal, keeping it", {

  with_session(
    {
      a <- reactiveVal(1)
      x <- reactives(a = a)

      slot_runs <- 0
      value_runs <- 0

      observe(
        {
          slot_runs <<- slot_runs + 1
          x["a"]
        }
      )

      observe(
        {
          value_runs <<- value_runs + 1
          x$a
        }
      )

      session$flushReact()

      x$a <- 1
      session$flushReact()

      expect_identical(value_runs, 1)

      x[["a"]] <- 2
      session$flushReact()

      expect_identical(x["a"], a)
      expect_identical(a(), 2)
      expect_identical(slot_runs, 1)
      expect_identical(value_runs, 2)

      x[[1]] <- 3
      expect_identical(a(), 3)
    }
  )
})

test_that("writing a value binds a reactiveVal holding it to a new slot", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      seen <- "unset"
      observe(seen <<- x["b"])
      session$flushReact()

      x$b <- 2
      session$flushReact()

      expect_s3_class(seen, "reactiveVal")
      expect_identical(seen, x["b"])
      expect_identical(x$b, 2)

      x[[c(p = "c")]] <- 3
      x[[factor("d")]] <- 4

      expect_identical(names(x), c("a", "b", "c", "d"))
    }
  )
})

test_that("assigning NULL with $<- or [[<- stores NULL", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      x$a <- NULL
      x[["b"]] <- NULL

      expect_identical(as_values(x), list(a = NULL, b = NULL))
    }
  )
})

test_that("a reactive is written as a value rather than bound", {

  with_session(
    {
      a <- reactiveVal(1)
      b <- reactive(2)
      x <- reactive_vals()

      x$a <- a
      x[["b"]] <- b

      expect_identical(x$a, a)
      expect_identical(x[["b"]], b)
      expect_false(identical(x["a"], a))

      x$a <- b
      expect_identical(x$a, b)
    }
  )
})

test_that("a slot holding a reactive expression can't be written", {

  with_session(
    {
      b <- reactive(2)
      x <- reactives(b = b)

      expect_error(x$b <- 3, class = "reactives_not_reactive_val")
      expect_error(x[["b"]] <- NULL, class = "reactives_not_reactive_val")
      expect_error(x[[1]] <- 3, class = "reactives_not_reactive_val")
      expect_identical(x["b"], b)

      x$a <- 1
      expect_identical(as_values(x), list(b = 2, a = 1))
    }
  )
})

test_that("writing creates a slot's reactiveVal in the collection's session", {

  with_session(
    {
      x <- reactive_vals()

      moduleServer(
        "child",
        function(input, output, session) {
          x$a <- 1
          x[[2]] <- 2
          append_value(x, 3)
        }
      )
      session$destroy("child")

      expect_identical(as_values(x), list(a = 1, 2, 3))
    }
  )
})

test_that("binding, removing and writing make the caller depend on nothing", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          x["c"] <- reactiveVal(runs)
          x[2] <- NULL
          x$d <- runs
          x[[1]] <- runs
        }
      )
      session$flushReact()

      x$a <- 10
      x$c <- 20
      x$d <- 30
      x["e"] <- reactiveVal(5)
      reorder(x, c("e", "d", "c", "a"))
      session$flushReact()

      expect_identical(runs, 1)
    }
  )
})

test_that("writing a value works outside a reactive context", {

  x <- reactive_vals(a = 1)

  x$a <- 2
  x[["b"]] <- 3
  x[[3]] <- 4

  expect_identical(isolate(as_values(x)), list(a = 2, b = 3, 4))
})

test_that("unnamed slots are read and removed by position", {

  with_session(
    {
      a <- reactiveVal("a")
      u1 <- reactiveVal("u1")
      u2 <- reactiveVal("u2")

      x <- reactives(u1, a = a, u2)

      expect_identical(names(x), c("", "a", ""))
      expect_identical(x[1], u1)
      expect_identical(x[3], u2)
      expect_identical(x[[3]], "u2")
      expect_identical(as.list(x), list(u1, a = a, u2))

      x[1] <- NULL
      expect_identical(names(x), c("a", ""))
      expect_identical(x[2], u2)

      expect_null(names(reactives(u1, u2)))
    }
  )
})

test_that("assigning one past the end appends an unnamed slot", {

  with_session(
    {
      x <- reactive_vals(a = 1)
      u <- reactiveVal("u")

      x[length(x) + 1] <- u

      expect_identical(names(x), c("a", ""))
      expect_identical(x[2], u)

      x[length(x) + 1] <- NULL
      expect_identical(length(x), 2L)

      x[[length(x) + 1]] <- "v"
      x[[length(x) + 1]] <- NULL

      expect_identical(names(x), c("a", "", "", ""))
      expect_identical(as_values(x), list(a = 1, "u", "v", NULL))
    }
  )
})

test_that("append_reactive() and append_value() add an unnamed slot", {

  with_session(
    {
      x <- reactives(a = reactiveVal(1))
      r <- reactive(2)

      expect_invisible(append_reactive(x, r))
      expect_invisible(append_value(x, 3))
      expect_identical(append_value(x, NULL), x)

      expect_identical(names(x), c("a", "", "", ""))
      expect_identical(x[2], r)
      expect_s3_class(x[3], "reactiveVal")
      expect_identical(as_values(x), list(a = 1, 2, 3, NULL))
    }
  )
})

test_that("appending makes the caller depend on nothing", {

  with_session(
    {
      x <- reactive_vals()

      runs <- 0
      observe(
        {
          runs <<- runs + 1

          if (runs <= 3) {
            append_reactive(x, reactiveVal(runs))
            append_value(x, runs)
          }
        }
      )
      session$flushReact()

      x[[1]] <- 10
      x[2] <- reactiveVal(20)
      append_value(x, 30)
      session$flushReact()

      expect_identical(runs, 1)
      expect_identical(as_values(x), list(10, 20, 30))
    }
  )
})

test_that("appending makes the checks of binding and writing", {

  with_session(
    {
      x <- reactive_vals(a = 1)
      y <- reactive_exprs(b = reactive(2))

      expect_error(
        append_reactive(x, reactive(3)),
        class = "reactives_not_reactive_val"
      )
      expect_error(
        append_reactive(y, reactiveVal(3)),
        class = "reactives_not_reactive_expr"
      )
      expect_error(
        append_reactive(reactives(), NULL),
        class = "reactives_not_reactive"
      )
      expect_error(append_value(y, 3), class = "reactives_not_reactive_val")

      expect_error(
        append_reactive(list(), reactiveVal(3)),
        class = "reactives_not_collection"
      )
      expect_error(append_value(list(), 3), class = "reactives_not_collection")

      expect_identical(as_values(x), list(a = 1))
      expect_identical(as_values(y), list(b = 2))
    }
  )
})

test_that("positions out of range and malformed indices are errors", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      expect_error(x[2], class = "reactives_out_of_bounds")
      expect_error(x[[2]], class = "reactives_out_of_bounds")
      expect_error(x[3] <- reactiveVal(1), class = "reactives_out_of_bounds")
      expect_error(x[[3]] <- 1, class = "reactives_out_of_bounds")
      expect_error(x[[c("a", "b")]], class = "reactives_bad_index")
      expect_error(x[[c("a", "b")]] <- 1, class = "reactives_bad_index")
      expect_error(x[[1.5]], class = "reactives_bad_index")
      expect_error(x[1.5] <- NULL, class = "reactives_bad_index")
      expect_error(x[[0]], class = "reactives_bad_index")
      expect_error(x[0], class = "reactives_bad_index")
      expect_error(x[[-1]], class = "reactives_bad_index")
      expect_error(x[[TRUE]], class = "reactives_bad_index")
      expect_error(x[TRUE], class = "reactives_bad_index")
      expect_error(x[[NA_integer_]], class = "reactives_bad_index")
      expect_error(x[[""]], class = "reactives_bad_index")
      expect_error(x[[""]] <- 1, class = "reactives_bad_index")
      expect_error(x[NA_character_], class = "reactives_bad_index")
    }
  )
})

test_that("[ and [<- take a single slot, and renaming is an error", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      expect_error(x[c("a", "b")], class = "reactives_unsupported")
      expect_error(x[1:2], class = "reactives_unsupported")
      expect_error(x[character()], class = "reactives_unsupported")
      expect_error(x[], class = "reactives_unsupported")

      expect_error(x[c("a", "b")] <- NULL, class = "reactives_unsupported")
      expect_error(x[] <- list(), class = "reactives_unsupported")

      expect_error(names(x) <- c("a", "z"), class = "reactives_unsupported")
      expect_error(names(x) <- NULL, class = "reactives_unsupported")

      expect_identical(names(x), c("a", "b"))
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
      expect_identical(x[2], u)

      expect_error(
        reorder(x, c("a", "")),
        class = "reactives_bad_order"
      )
      expect_error(reorder(x, 1), class = "reactives_bad_order")
      expect_error(reorder(x, c(1, 1)), class = "reactives_bad_order")
    }
  )
})

test_that("reorder() rejects zero, fractional and negative positions", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      expect_error(reorder(x, c(0, 2, 1)), class = "reactives_bad_order")
      expect_error(reorder(x, c(2.5, 1.5)), class = "reactives_bad_order")
      expect_error(reorder(x, -3), class = "reactives_bad_order")

      reorder(x, c(p = 2, q = 1))
      expect_identical(names(x), c("b", "a"))
    }
  )
})

test_that("reorder() ignores the names and factor class of the order", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          names(x)
        }
      )
      session$flushReact()

      reorder(x, c(p = "a", q = "b"))
      session$flushReact()

      expect_identical(runs, 1)

      reorder(x, c(p = "b", q = "a"))
      expect_identical(names(x), c("b", "a"))

      reorder(x, factor(c("a", "b")))
      expect_identical(names(x), c("a", "b"))
      expect_identical(as_values(x), list(a = 1, b = 2))
    }
  )
})

test_that("reorder() is exported as the generic from stats", {

  expect_identical(reactives::reorder, stats::reorder)

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      reactives::reorder(x, c("b", "a"))
      expect_identical(names(x), c("b", "a"))
    }
  )
})

test_that("binding a slot ignores the names of its subscript", {

  with_session(
    {
      x <- reactive_vals(a = 1)
      x[c(p = "b")] <- reactiveVal(2)

      expect_identical(names(x), c("a", "b"))

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          names(x)
        }
      )
      session$flushReact()

      reorder(x, c("a", "b"))
      session$flushReact()

      expect_identical(runs, 1)
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

  x["b"] <- reactiveVal(3)
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

      x["a"] <- reactiveVal(2)
      x["c"] <- reactiveVal(3)
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

      x["b"] <- reactiveVal(2)
      x$a <- 3
      z <- reorder(x, c("b", "a"))

      expect_identical(names(y), c("b", "a"))
      expect_identical(y["b"], x["b"])
      expect_identical(y$a, 3)
      expect_identical(z, x)
    }
  )
})

test_that("a slot first read in a module survives the module", {

  with_session(
    {
      x <- reactives()

      moduleServer("child", function(input, output, session) x["a"])
      session$destroy("child")

      a <- reactiveVal(1)
      x["a"] <- a

      expect_identical(x["a"], a)
    }
  )
})

test_that("a collection is destroyed along with its module", {

  with_session(
    {
      x <- moduleServer("child", function(input, output, session) reactives())
      x["a"] <- reactiveVal(1)
      session$destroy("child")

      expect_error(x["a"], class = "shiny.destroyed.error")
      expect_error(x[1], class = "shiny.destroyed.error")
      expect_error(x$a, class = "shiny.destroyed.error")
      expect_error(x[[1]], class = "shiny.destroyed.error")
      expect_error(names(x), class = "shiny.destroyed.error")
      expect_error(length(x), class = "shiny.destroyed.error")
    }
  )
})

test_that("binding a slot of a destroyed collection fails", {

  with_session(
    {
      x <- moduleServer("child", function(input, output, session) reactives())
      x["a"] <- reactiveVal(1)
      session$destroy("child")

      expect_error(x["a"] <- reactiveVal(2), class = "shiny.destroyed.error")
      expect_error(x[1] <- reactiveVal(2), class = "shiny.destroyed.error")
      expect_error(x["b"] <- reactiveVal(2), class = "shiny.destroyed.error")
      expect_error(x[2] <- reactiveVal(2), class = "shiny.destroyed.error")
    }
  )
})

test_that("printing or changing a destroyed collection fails", {

  with_session(
    {
      x <- moduleServer("child", function(input, output, session) reactives())
      x["a"] <- reactiveVal(1)
      session$destroy("child")

      expect_error(x["a"] <- NULL, class = "shiny.destroyed.error")
      expect_error(x["b"] <- NULL, class = "shiny.destroyed.error")
      expect_error(reorder(x, "a"), class = "shiny.destroyed.error")

      expect_error(x$a <- 2, class = "shiny.destroyed.error")
      expect_error(x[[1]] <- 2, class = "shiny.destroyed.error")
      expect_error(x$b <- 2, class = "shiny.destroyed.error")
      expect_error(x[[2]] <- 2, class = "shiny.destroyed.error")

      expect_error(
        append_reactive(x, reactiveVal(2)),
        class = "shiny.destroyed.error"
      )
      expect_error(append_value(x, 2), class = "shiny.destroyed.error")

      expect_error(format(x), class = "shiny.destroyed.error")
      expect_error(print(x), class = "shiny.destroyed.error")
      expect_error(str(x), class = "shiny.destroyed.error")
    }
  )
})

test_that("reactive_vals() only accepts reactiveVal slots", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      expect_s3_class(x, "reactive_vals")
      expect_s3_class(x, "reactives")

      expect_error(x["b"] <- reactive(2), class = "reactives_not_reactive_val")
      expect_error(x["b"] <- 2, class = "reactives_not_reactive_val")

      x["b"] <- reactiveVal(2)
      expect_identical(x$b, 2)

      y <- reactives(a = reactiveVal(1), b = reactive(2))
      expect_false(inherits(y, "reactive_vals"))
    }
  )
})

test_that("reactive_exprs() only accepts reactive expressions", {

  with_session(
    {
      x <- reactive_exprs(a = reactive(1), b = NULL)

      expect_s3_class(x, "reactive_exprs")
      expect_s3_class(x, "reactives")
      expect_identical(names(x), "a")

      expect_error(
        x["b"] <- reactiveVal(2),
        class = "reactives_not_reactive_expr"
      )
      expect_error(x["b"] <- 2, class = "reactives_not_reactive_expr")
      expect_error(
        reactive_exprs(a = reactiveVal(1)),
        class = "reactives_not_reactive_expr"
      )
      expect_error(reactive_exprs(a = 1), class = "reactives_not_reactive_expr")

      x["b"] <- reactive(2)
      x["a"] <- NULL
      expect_identical(as_values(x), list(b = 2))
    }
  )
})

test_that("reactive_exprs() takes each kind of reactive expression", {

  with_session(
    {
      r <- reactiveVal(1)

      x <- reactive_exprs(
        reactive(r()),
        eventReactive(r(), r()),
        debounce(r, 10),
        throttle(r, 10),
        bindEvent(reactive(r()), r()),
        bindCache(reactive(r()), r()),
        reactivePoll(1000, session, r, r),
        reactiveFileReader(1000, session, "file", readLines)
      )

      expect_identical(length(x), 8L)
    }
  )
})

test_that("no value can be written to a reactive_exprs collection", {

  with_session(
    {
      x <- reactive_exprs(a = reactive(1), b = reactive(2))

      expect_error(x$a <- 3, class = "reactives_not_reactive_val")
      expect_error(x$c <- 3, class = "reactives_not_reactive_val")
      expect_error(x[["c"]] <- NULL, class = "reactives_not_reactive_val")
      expect_error(x[[1]] <- 3, class = "reactives_not_reactive_val")
      expect_error(x[[3]] <- 3, class = "reactives_not_reactive_val")

      expect_identical(as_values(x), list(a = 1, b = 2))
    }
  )
})

test_that("is_reactives() is TRUE for a collection of any kind only", {

  expect_true(is_reactives(reactives()))
  expect_true(is_reactives(reactive_vals(a = 1)))
  expect_true(is_reactives(reactive_exprs()))

  expect_false(is_reactives(list()))
  expect_false(is_reactives(shiny::reactiveValues()))
  expect_false(is_reactives(reactiveVal(1)))
})

test_that("lapply() goes through the reactives, and Map() through the values", {

  with_session(
    {
      x <- reactives(a = reactiveVal(1), reactive(2))

      expect_identical(lapply(x, is.reactive), list(a = TRUE, TRUE))
      expect_identical(Map(identity, x), list(a = 1, 2))
    }
  )
})

test_that("reactlog labels each slot with the class and key", {

  labels <- reactlog_labels(
    {
      x <- reactives(reactiveVal(1), a = reactiveVal(2), reactiveVal(3))
      y <- reactive_vals(b = 1)
      z <- reactive_exprs(c = reactive(1))

      isolate(
        {
          x[1]
          x["a"]
          x[3]
          y["b"]
          z["c"]
          as.list(x)
        }
      )
    }
  )

  expect_identical(
    setdiff(
      c(
        "names(reactives)", "reactives$a", "reactives$...1",
        "reactives$...2", "names(reactive_vals)", "reactive_vals$b",
        "names(reactive_exprs)", "reactive_exprs$c", "reactives[]"
      ),
      labels
    ),
    character()
  )

  expect_false("reactives$...3" %in% labels)
})

test_that("reactlog labels a read by position with its key, and length() too", {

  labels <- reactlog_labels(
    {
      x <- reactives(a = reactiveVal(1), reactiveVal(2))

      isolate(
        {
          x[2]
          length(x)
        }
      )
    }
  )

  expect_true(all(c("reactives$...1", "length(reactives)") %in% labels))
  expect_false("reactives$a" %in% labels)
})

test_that("reactive_vals() labels each value after its slot", {

  expect_identical(
    reactlog_labels(reactive_vals(1, a = 2, NULL)),
    c(
      "names(reactive_vals)", "reactive_vals$...1()", "reactive_vals$a()",
      "reactive_vals$...2()"
    )
  )
})

test_that("writing a value labels its reactiveVal as reactive_vals() does", {

  x <- reactives()
  y <- reactive_vals()

  expect_identical(
    reactlog_labels(
      {
        x$a <- 1
        y[["b"]] <- 2
        y[[2]] <- 3
        append_value(y, 4)
      }
    ),
    c(
      "reactives$a()", "reactive_vals$b()", "reactive_vals$...1()",
      "reactive_vals$...2()"
    )
  )
})

test_that("a key's cell is created when the key is first read", {

  labels <- reactlog_labels(
    {
      x <- reactives(a = reactiveVal(1), reactiveVal(2))
      isolate(as.list(x))
      capture.output(str(x), print(x))
    }
  )

  expect_identical(grep("$", labels, fixed = TRUE, value = TRUE), character())

  expect_identical(reactlog_labels(isolate(x["a"])), "reactives$a")
})

test_that("a key's cell is deleted once its slot is removed", {

  x <- reactives(a = reactiveVal(1), b = reactiveVal(2))
  isolate(list(x["a"], x["b"]))

  x["a"] <- NULL
  x[1] <- NULL

  expect_identical(
    reactlog_labels(isolate(list(x["a"], x["b"]))),
    c("reactives$a", "reactives$b")
  )
})

test_that("the first read of a key, length() or every slot writes no cell", {

  x <- reactives(a = reactiveVal(1))

  expect_identical(
    reactlog_writes(isolate(list(x["a"], x$b, length(x), as.list(x)))),
    character()
  )
})

test_that("replacing a slot releases its old reactive", {

  with_session(
    {
      released <- FALSE
      mark <- function(e) released <<- TRUE

      tracked <- new.env()
      reg.finalizer(tracked, mark)

      x <- reactives(a = reactiveVal(tracked))
      rm(tracked)

      x["a"]
      x[1]
      x["a"] <- reactiveVal(2)
      gc()

      expect_true(released)
    }
  )
})

test_that("a collection made in a session is released once dropped", {

  with_session(
    {
      released <- FALSE
      mark <- function(e) released <<- TRUE

      x <- reactives(a = reactiveVal(1))
      as.list(x)

      reg.finalizer(.subset2(x, "state"), mark)
      rm(x)
      gc()

      expect_true(released)
    }
  )
})

test_that("a reader by position re-runs when another slot moves there", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2, c = 3)

      seen <- NULL
      observe(seen <<- x[1])
      session$flushReact()

      reorder(x, c("c", "a", "b"))
      session$flushReact()

      expect_identical(seen, x["c"])
    }
  )
})

test_that("a reader by position re-runs when its slot is replaced or removed", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      seen <- NULL
      observe(seen <<- x[1])
      session$flushReact()

      a <- reactiveVal(10)
      x["a"] <- a
      session$flushReact()

      expect_identical(seen, a)

      x["a"] <- NULL
      session$flushReact()

      expect_identical(seen, x["b"])
    }
  )
})

test_that("a reader of a value by position re-runs when the value changes", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      seen <- NULL
      observe(seen <<- x[[1]])
      session$flushReact()

      x$a <- 10
      session$flushReact()

      expect_identical(seen, 10)

      reorder(x, c("b", "a"))
      session$flushReact()

      expect_identical(seen, 2)
    }
  )
})

test_that("a reader beyond the last slot re-runs once a slot arrives there", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      seen <- "unset"
      observe(
        seen <<- tryCatch(x[2], reactives_out_of_bounds = function(e) NULL)
      )
      session$flushReact()

      expect_null(seen)

      u <- reactiveVal(2)
      x[2] <- u
      session$flushReact()

      expect_identical(seen, u)

      x[2] <- NULL
      session$flushReact()

      expect_null(seen)
    }
  )
})

test_that("length() re-runs when slots are added or removed, and only then", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          length(x)
        }
      )
      session$flushReact()

      reorder(x, c("b", "a"))
      x["a"] <- reactiveVal(3)
      x$b <- 4
      session$flushReact()

      expect_identical(runs, 1)

      x["r"] <- reactiveVal(4)
      session$flushReact()

      expect_identical(runs, 2)

      x["a"] <- NULL
      session$flushReact()

      expect_identical(runs, 3)
    }
  )
})
