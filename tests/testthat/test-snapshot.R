test_that("a snapshot outlives the module it was taken in", {

  with_session(
    {
      res <- moduleServer(
        "child",
        function(input, output, session) {
          x <- reactives(a = reactiveVal(1), b = reactive(2), reactiveVal(3))
          list(x = x, snap = snapshot_reactives(x))
        }
      )

      session$destroy("child")

      expect_error(res$x$a, class = "shiny.destroyed.error")

      expect_identical(names(res$snap), c("a", "b", ""))
      expect_identical(slot_values(res$snap), list(a = 1, b = 2, 3))
    }
  )
})

test_that("a snapshot holds the current values apart from the original", {

  with_session(
    {
      x <- reactive_vals(a = 1, b = 2)
      snap <- snapshot_reactives(x)

      expect_s3_class(snap, "reactive_vals")

      x$a(10)
      snap$b(20)
      x$c <- reactiveVal(3)

      expect_identical(slot_values(snap), list(a = 1, b = 20))
      expect_identical(slot_values(x), list(a = 10, b = 2, c = 3))
    }
  )
})

test_that("a snapshot keeps the kind of each slot and replays its errors", {

  with_session(
    {
      x <- reactives(
        stored = reactiveVal(1),
        computed = reactive(2),
        failing = reactive(stop("boom")),
        silent = reactive(req(FALSE))
      )

      snap <- snapshot_reactives(x)

      expect_s3_class(snap$stored, "reactiveVal")
      expect_s3_class(snap$computed, "reactiveExpr")
      expect_identical(snap$computed(), 2)

      expect_error(snap$failing(), "boom")
      expect_error(snap$silent(), class = "shiny.silent.error")
    }
  )
})

test_that("taking a snapshot makes the caller depend on nothing", {

  with_session(
    {
      x <- reactive_vals(a = 1)

      runs <- 0
      observe(
        {
          runs <<- runs + 1
          snapshot_reactives(x)
        }
      )
      session$flushReact()

      x$a(2)
      x$b <- reactiveVal(3)
      session$flushReact()

      expect_identical(runs, 1)
    }
  )
})

test_that("snapshot_reactives() needs a collection", {
  expect_error(snapshot_reactives(list()), class = "reactives_not_collection")
})
