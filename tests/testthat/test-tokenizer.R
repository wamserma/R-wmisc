context("File tokenizer")

test_that("call without filename gives warning and object as with closed file",{
          expect_warning(tok<-Tokenizer$new())
          expect_output(tok$print(),"No file open. Please create a new object for reading.")
          })

#this fails as normalizPath seems to return a closure in that case
#test_that("call with inexistent file gives warning and object as with closed file",{
#  expect_warning(tok<-Tokenizer$new("imnothere.invalid"))
  #expect_output(tok$print(),"No file open. Please create a new object for reading.")
#})

test_that("opening and closing a file works",{
  tok<-Tokenizer$new("token.txt")
  expect_output(tok$print(),"File open.*token.txt")
  tok$close()
  expect_output(tok$print(),"No file open. Please create a new object for reading.")
})

test_that("reading from a closed file returns NA",{
  expect_warning(tok<-Tokenizer$new())
  expect_identical(tok$nextToken(),NA)
})

test_that("new tokenizer has default delimiters",{
  expect_warning(tok<-Tokenizer$new())
  expect_identical(tok$getDelimiters(),as.integer(c(9,10,13,32)))
})

test_that("modifying delimiters works",{
  expect_warning(tok<-Tokenizer$new())
  tmp<-tok$getDelimiters()
  tmp[3]<-as.integer(42)
  tok$setDelimiters(tmp)
  expect_identical(tok$getDelimiters(),as.integer(c(9,10,42,32)))
})

test_that("Only ASCII-characters (numerical value < 256) accepted.",{
  expect_warning(tok<-Tokenizer$new())
  expect_error(tok$setDelimiters(c(3,4,5,12345)),"Only ASCII.*")
  expect_error(tok$setDelimiters(c(3,4,-5,8)),"Only ASCII.*")
  expect_error(tok$setDelimiters(c(3,4,7.2,8)),"Only ASCII.*")
})

test_that("reading tokens works as expected",{
  tok<-Tokenizer$new("token.txt")
  expect_identical(tok$nextToken(),"Hi,")
  expect_identical(tok$nextToken(),"use")
  expect_identical(tok$nextToken(),"me")
  tok$close()
})


test_that("EOF is detected",{
  tok<-Tokenizer$new("token.txt",FALSE)
  for (i in 1:41) token <- tok$nextToken()
  expect_equal(token,"newline.")
  expect_equal(tok$nextToken(),NA)
  tok$close()
})

test_that("emptiy tokens are skipped",{
  tok<-Tokenizer$new("token.txt")
  for (i in 1:7) token <- tok$nextToken()
  expect_equal(token,"tokeniser.")
  tok$close()
  tok<-Tokenizer$new("token.txt",FALSE)
  for (i in 1:7) token <- tok$nextToken()
  expect_equal(token,"")
  tok$close()
})

