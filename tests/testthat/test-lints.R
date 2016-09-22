if (requireNamespace("lintr",quietly = TRUE)) {
  context("lints")
  test_that("Package has good style (no lints)", {
    lintr::expect_lint_free()
  })
}
