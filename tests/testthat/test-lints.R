context("Lints")

if (requireNamespace("lintr",quietly = TRUE)) {
  test_that("Package has good style (no lints)", {
    lintr::expect_lint_free()
  })
}
