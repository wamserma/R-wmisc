context("File mapping")

# testing the C-Level functions

test_that("mmapping of inexistent file returns List-NA", {
  expect_warning(fd<-CWmisc_mmap("nothere.txt"))
  testthat::expect_null(fd[1][[1]])
  testthat::expect_is(fd,"list")
})

test_that("mmapping of empty file returns List-NA", {
  expect_warning(fd<-CWmisc_mmap("empty.txt"))
  testthat::expect_null(fd[1][[1]])
  testthat::expect_is(fd,"list")
})
