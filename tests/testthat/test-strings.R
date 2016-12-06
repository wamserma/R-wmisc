context("Strings")

test_that("head works",{
  expect_identical(strHead("gnu"),"g")
  expect_identical(strHead(""),"")
  expect_error(strHead(c("foo","bar")))
})

test_that("head to lower works",{
  expect_identical(strHeadLower("Gnu"),"g")
  expect_identical(strHeadLower(""),"")
  expect_error(strHeadLower(c("foo","bar")))
})

test_that("tail works",{
  expect_identical(strTail("Gnat"),"nat")
  expect_identical(strTail(""),"")
  expect_error(strTail(c("foo","bar")))
})

test_that("take works",{
  expect_identical(strTake("Gnat",0),"")
  expect_identical(strTake("Gnat",2),"Gn")
  expect_identical(strTake("Gnat",8),"Gnat")
  expect_identical(strTake("",3),"")
  expect_error(strTake(c("foo","bar"),3))
})

test_that("drop works",{
  expect_identical(strDrop("Gnat",0),"Gnat")
  expect_identical(strDrop("Gnat",2),"at")
  expect_identical(strDrop("Gnat",8),"")
  expect_identical(strDrop("",3),"")
  expect_error(strDrop(c("foo","bar"),3))
})
