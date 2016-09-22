context("File tokenizer")

test_that("call without filename gives warning and object as with closed file", {
          expect_warning(tok<-Tokenizer$new())
          expect_output(tok$print(), "No file open. Please create a new object for reading.")
          })

test_that("call with inexistent file gives warning and object as with closed file", {
  expect_warning(tok<-Tokenizer$new("nofile.txt"))
  expect_output(tok$print(), "No file open. Please create a new object for reading.")
})

test_that("opening and closing a file works", {
  tok<-Tokenizer$new("token.txt")
  expect_output(tok$print(), "File open.*token.txt")
  tok$close()
  expect_output(tok$print(), "No file open. Please create a new object for reading.")
})

test_that("reading from a closed file returns NA", {
  expect_warning(tok<-Tokenizer$new())
  expect_identical(tok$nextToken(), NA)
})

test_that("new tokenizer has default delimiters", {
  expect_warning(tok<-Tokenizer$new())
  expect_identical(tok$getDelimiters(), as.integer(c(9, 10, 13, 32)))
})

test_that("modifying delimiters works", {
  expect_warning(tok<-Tokenizer$new())
  tmp<-tok$getDelimiters()
  tmp[3]<-as.integer(42)
  tok$setDelimiters(tmp)
  expect_identical(tok$getDelimiters(), as.integer(c(9, 10, 42, 32)))
})

test_that("Only ASCII-characters (numerical value < 256) accepted.", {
  expect_warning(tok<-Tokenizer$new())
  expect_error(tok$setDelimiters(c(3, 4, 5, 12345)), "Only ASCII.*")
  expect_identical(tok$getDelimiters(), as.integer(c(9, 10, 13, 32)))
  expect_error(tok$setDelimiters(as.integer(c(3, 4, 5, 12345))), "Only ASCII.*")
  expect_identical(tok$getDelimiters(), as.integer(c(9, 10, 13, 32)))
  expect_error(tok$setDelimiters(c(3, 4, -5, 8)), "Only ASCII.*")
  expect_identical(tok$getDelimiters(), as.integer(c(9, 10, 13, 32)))
  expect_error(tok$setDelimiters(c(3, 4, 7.2, 8)), "Only ASCII.*")
  expect_identical(tok$getDelimiters(), as.integer(c(9, 10, 13, 32)))
})

test_that("reading tokens works as expected", {
  tok<-Tokenizer$new("token.txt")
  expect_identical(tok$nextToken(), "Hi,")
  expect_identical(tok$nextToken(), "use")
  expect_identical(tok$nextToken(), "me")
  tok$close()
})


test_that("EOF is detected", {
  tok<-Tokenizer$new("token.txt", FALSE)
  i<-0
  token <- tok$nextToken()
  while ( (token != "without") && (i < 100)){
    token <- tok$nextToken()
    i<-i+1
  }
  expect_equal(tok$nextToken(), "newline.")
  expect_equal(tok$nextToken(), NA)
  expect_lte(i, 41)
  tok$close()
})

test_that("empty tokens are skipped", {
  tok<-Tokenizer$new("token.txt")
  tok$setDelimiters(c(0x69L, 0x2cL, 0x9L))
  expect_equal(tok$nextToken(), "H")
  expect_equal(tok$nextToken(), " use me to")
  tok$close()
})

test_that("empty tokens are not skipped on request", {
  tok<-Tokenizer$new("token.txt", FALSE)
  tok$setDelimiters(c(0x69L, 0x2cL, 0x9L))
  expect_equal(tok$nextToken(), "H")
  expect_equal(tok$nextToken(), "")
  expect_equal(tok$nextToken(), " use me to")
  tok$close()
})
