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

test_that("EOF is detected when the last token is silent", {
  tok<-Tokenizer$new("token.txt")
  i<-0
  tok$setDelimiters(as.integer(charToRaw(".")))
  token <- "foo"
  while ( (!is.na(token)) && (i < 100)){
    token <- tok$nextToken()
    i<-i+1
  }
  expect_equal(i, 5)
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

## testing the reading functions
test_that("readTokens() works", {
  ref <-
    c("Hi,", "use", "me", "to", "test", "the", "tokeniser.", "Maybe,use;comma,or;semicolon",
      "or", "XZG", "or", "whatever", "you", "like.", "I", "use", "Windows",
      "line", "endings", "(CR+LF)", "so", "you", "can", "test", "tokenising",
      "on", "different", "combinations.", "xx", "yy", "zz", "EOF",
      "without", "newline.")
  s<-readTokens(Tokenizer$new("token.txt"))
  expect_equal(s, ref)
  t<-readTokens("token.txt")
  expect_equal(t, ref)
  tok<-Tokenizer$new("token.txt")
  tok$nextToken()
  u<-readTokens(tok)
  expect_equal(u, ref[-1])
})

test_that("readTokens() takes custom delimiter", {
  ref<-c("Hi", " use me to\ttest the\r\ntokeniser.\r\nMaybe", "use", "comma",
        "or", "semicolon or XZG or whatever you like.\r\nI use Windows line endings (CR",
        "LF) so you can test tokenising on different combinations.\r\nxx\r\nyy\r\nzz\r\nEOF without newline.")
  s<-readTokens(Tokenizer$new("token.txt"),delims=as.integer(charToRaw(",;+")))
  expect_equal(s, ref)
  t<-readTokens("token.txt",delims=as.integer(charToRaw(",;+")))
  expect_equal(t, ref)
  ref[2]<-"use me to\ttest the\r\ntokeniser.\r\nMaybe"
  tok<-Tokenizer$new("token.txt")
  tok$nextToken()
  u<-readTokens(tok,as.integer(charToRaw(",;+")))
  expect_equal(u, ref[-1])
})

test_that("readTokens() handles invalid input gracefully", {
  s<-readTokens()
  expect_identical(s, as.character(c()))
  t<-readTokens(3)
  expect_identical(t, as.character(c()))
  expect_warning(u<-readTokens("foo"))
  expect_identical(u, as.character(c()))
})

test_that("readlines() works", {
  s<-readlines()
  expect_identical(s, as.character(c()))
  ref<-c("Hi, use me to\ttest the", "tokeniser.",
    "Maybe,use;comma,or;semicolon or XZG or whatever you like.",
    "I use Windows line endings (CR+LF) so you can test tokenising on different combinations.",
    "xx", "yy", "zz", "EOF without newline.")
  t<-readlines("token.txt")
  expect_identical(t, ref)
  expect_warning(u<-readlines("foo"))
  expect_identical(u, as.character(c()))
})
