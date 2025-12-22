test_that("Package does not contain roxygen comments", {
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

  has_roxygen <- vapply(
    r_files,
    function(f) any(grepl("^#'", readLines(f, warn = FALSE))),
    logical(1)
  )

  expect_false(
    any(has_roxygen),
    info = "Roxygen comments detected. Documentation should be maintained manually in man/."
  )
})
