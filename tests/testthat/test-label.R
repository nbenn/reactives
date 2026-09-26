test_that("reactlog shows each slot under the collection's variable", {

  labels <- reactlog_labels(
    with_session(
      {
        x <- reactives(a = reactiveVal(1), reactiveVal(2))
        y <- x["a"]
      }
    )
  )

  expect_identical(
    setdiff(c("names(x)", "x$a", "x$...1", "names(y)", "y$a"), labels),
    character()
  )
})

test_that("a collection created without source references is numbered", {

  labels <- reactlog_labels(do.call(reactive_vals, list(b = 1, 2)))

  expect_match(labels, "^names\\(reactive_vals[0-9]+\\)$", all = FALSE)
  expect_match(labels, "^reactive_vals[0-9]+\\$b$", all = FALSE)
  expect_match(labels, "^reactive_vals[0-9]+\\$\\.\\.\\.1$", all = FALSE)
})

test_that("the name is read from an assignment only", {

  srcref <- function(code) {
    attr(parse(text = code, keep.source = TRUE), "srcref")[[1L]]
  }

  expect_identical(assigned_name(srcref("x <- reactives()")), "x")
  expect_identical(assigned_name(srcref("x = reactives()")), "x")
  expect_identical(assigned_name(srcref("x <<- reactives()")), "x")
  expect_identical(assigned_name(srcref("app$x <- reactives()")), "app$x")

  expect_null(assigned_name(srcref("f(reactives())")))
  expect_null(assigned_name(srcref("x == reactives()")))
  expect_null(assigned_name(NULL))
})
