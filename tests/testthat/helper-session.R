# Since testServer() quotes its `expr`, splice the caller's expression in
# rather than forwarding the promise, which would evaluate outside the session.
with_session <- function(expr) {
  eval(
    substitute(shiny::testServer(function(input, output, session) NULL, expr)),
    parent.frame()
  )
}
