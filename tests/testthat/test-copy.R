test_that("a deep copy holds its stored values apart from the original", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)
      y <- copy(x, deep = TRUE)

      expect_s3_class(y$a, "reactiveVal")
      expect_false(identical(y$a, x$a))

      y$a(10)
      x$b(20)

      expect_identical(slot_values(x), list(a = 1, b = 20))
      expect_identical(slot_values(y), list(a = 10, b = 2))
    }
  )
})

test_that("a deep copy shares computed slots, which still read the original", {

  with_session(
    {
      ran <- FALSE
      x <- reactives(a = reactiveVal(1), b = reactive(ran <<- TRUE))
      x$c <- reactive(x$a() * 2)

      y <- copy(x, deep = TRUE)

      expect_false(ran)
      expect_identical(y$b, x$b)
      expect_identical(y$c, x$c)

      y$a(10)
      expect_identical(y$c(), 2)

      x$a(5)
      expect_identical(y$c(), 10)
    }
  )
})

test_that("a copy keeps the names, order and class", {

  with_session(
    {
      x <- reactive_vals(a = 1, 2, b = 3)
      reorder(x, c(3, 1, 2))

      y <- copy(x, deep = TRUE)

      expect_identical(class(y), c("reactive_vals", "reactives"))
      expect_identical(names(y), c("b", "a", ""))
      expect_identical(slot_values(y), list(b = 3, a = 1, 2))

      expect_identical(
        class(copy(reactives(a = reactive(1)), deep = TRUE)),
        "reactives"
      )
    }
  )
})

test_that("a shallow copy holds the same reactives, as x[] does", {

  with_session(
    {
      x <- reactives(a = reactiveVal(1), b = reactive(2), reactiveVal(3))
      y <- copy(x)

      expect_identical(class(y), class(x))
      expect_identical(as.list(y), as.list(x))

      y$a(10)
      expect_identical(x$a(), 10)

      y$c <- reactiveVal(4)
      expect_null(x$c)
    }
  )
})

test_that("a deep copy depends on the slots but not on their values", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          copy(x, deep = TRUE)
        }
      )
      session$flushReact()

      x$a(2)
      session$flushReact()

      expect_identical(runs, 1)

      x$b <- reactiveVal(3)
      session$flushReact()

      expect_identical(runs, 2)
    }
  )
})

test_that("a deep copy belongs to the module it is made in", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      y <- moduleServer(
        "child",
        function(input, output, session) copy(x, deep = TRUE)
      )
      a <- y$a

      session$destroy("child")

      expect_error(a(), class = "shiny.destroyed.error")
      expect_error(y$a, class = "shiny.destroyed.error")
      expect_identical(x$a(), 1)
    }
  )
})

test_that("a deep copy creates no cells until a key is read", {

  labels <- reactlog_labels(
    {
      x <- reactive_vals(a = 1, 2)
      y <- isolate(copy(x, deep = TRUE))
    }
  )

  expect_identical(grep("$", labels, fixed = TRUE, value = TRUE), character())

  expect_identical(reactlog_labels(isolate(y$a)), "reactive_vals$a")
})

test_that("copy() needs a collection and a single flag", {

  x <- reactives()

  expect_error(copy(list()), class = "reactives_not_collection")
  expect_error(copy(x, deep = NA), class = "reactives_bad_flag")
  expect_error(copy(x, deep = "yes"), class = "reactives_bad_flag")
})
