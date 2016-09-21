#' Create a \code{Tokenizer} to read string tokens from a memory mapped file.
#'
#' @description Reading and processing tokens from a text file usually is done in three steps: Load the file, cut into tokenst, act upon the resulting vector of strings.
#' The \code{Tokenizer} aims to simplify and streamline the process, when tokens must be processed in a sequential manner.
#' 
#' @details While the life-cycle of the \code{Tokenizer} still requires the user to act in three phases, it abstracts away the nasties of the file access, leverages the powers of the underlying operaring system for prefetching. Most of all, bookkeeping is much simpler: The user simply has to keep track of the object returned by the constructor and is free to pass it around between functions without caring for the current state. The \code{Tokenizer} will also try to close open files by itself, before it ist Garbage Collected.
#' 
#' The \code{Tokenizer} is object-oriented, so functions on any instance can be called in a OO style or in more imperative style:
#' \itemize{
#'   \item OO style: \code{tok$nextToken()}
#'   \item imperative style: \code{nextToken(tok)}
#' }
#' Both calls will give the same result. 
#' 
#' @docType class
#' @importFrom R6 R6Class
#' 
#' @section Methods:
#' 
#' \describe{
#'  \item{\code{new()}}{Create a new instance of a \code{Tokenizer}}
#'  \item{\code{nextToken()}}{Obtain the next token, that is the character vector from the character after the last delimiter up to the next delimiter from the current list of delimiters. It will return \code{NA} on all invocations once the EOF is reached.}
#'  \item{\code{setDelimiters()}}{Set the list of delimiters. It is given as an integer vector of (extended) ASCII-character values, i.e. in the range [0..255].}
#'  \item{\code{getDelimiters()}}{Get the current list of delimiters.}
#'  \item{\code{close()}}{Close the file behind the tokenizer. Future calls to \code{nextToken()} will return \code{NA}. It is considered good style to close the file manually to avoid to many open handles. The file will be closed automatically when there are no more references to the \code{Tokenizer} and it is garbage collected or upon exiting the R session.}
#'  \item{\code{print()}}{Prints the name of the currently opened file.}
#' }
#'
#' @usage 
#' # tok <- Tokenizer$new(filename=NA, skipEmptyTokens=TRUE)
#' # Tokenizer$getDelimiters()
#' # Tokenizer$setDelimiters(delims)
#' # Tokenizer$nextToken()
#' # Tokenizer$close()
#'
#' @param filename The file to open.
#' @param skipEmptyTokens set whether epty tokens ("") shall be skipped or returned
#' @param delims An integer vector holding the ASCII-codes of characters that serve as delimiters. If not set, it defaults to blank, tab, carriage return and linefeed (the last two together resemble a Windows newline).
#' @return A new Tokenizer object, backed by a memory mapped file and the delimiters set to the default values.
#' 
#' 
#' 
#' @examples
#' \dontrun{
#' tok<-Tokenizer$new("tokenfile.txt")
#' tok$nextToken()                                    # 
#' tok$print()                                        # or just 'tok'
#' tok$getDelimiters()
#' tok$setDelimiters(c(59L,0xaL))                     # new Delimiters: ';', newline
#' tok$setDelimiters(as.integer(charToRaw(";\n")))    # the same
#' tok$nextToken()
#' tok$setDelimiters(Tokenizer$new()$getDelimiters()) # reset to default
#' # while(!is.na(s<-tok$nextToken())) print(s)       # print the remaing tokens of file
#' tok$close()                                        # good style, but not required
#' }
#' @section Final Remarks:
#' 
#' While it may be tempting to clone a tokenizer object to split a file into different tokens from a given start position, this is not supported, as file state cannot be synchronized between the clones, leading to unpredictable results, when one of the clones closes the underlying shared file.
#' 
#' For efficiency reasons, \code{Tokenizer} will not re-stat the file once it is successfully opened. This means that especially a change of the file size can lead to unpredictable behaviour.
#' 
#' The sequence \code{\\cr\\lf} will be interpreted as two distinct tokens, if \code{skipEmptyTokens=FALSE}. The defualt setting is \code{TRUE}
#' @rdname Tokenizer
#' @format An \code{\link{R6Class}} generator object. 
#' @export

Tokenizer <- R6::R6Class("Tokenizer",
                  public = list(
                     initialize = function(filename = NA, skipEmptyTokens = TRUE) {
                      if (is.na(filename)) {
                        warning("No file given. Tokenizer will always return NA.", immediate. = T)
                        private$nofile <- TRUE
                      }
                      if (! private$nofile) {
                        filename <- normalizePath( as.character(filename) )
                        if (!file.exists(filename)) {
                          warning("No file at file location '", filename, "'. Tokenizer will always return NA.")
                          private$nofile <- TRUE
                        }
                      } 
                      if (! private$nofile) { 
                        private$fname <- filename
                        private$skipEmpty <- skipEmptyTokens
                        private$fd = CWmisc_mmap(filename)
                        if (is.null(private$fd[1][[1]])) private$nofile <- TRUE
                        else{
                          private$currentPtr <- private$fd$map
                        }
                      }
                      private$delims <- as.integer(c(9,10,13,32)) # tab, lf, cr, blank
					  reg.finalizer(self,function(e) self$close,onexit = TRUE)
                    }, # end initialize
                    setDelimiters = function(delims) {
                      if (all(delims<256) & all(delims>=0) & all(is.integer(delims)))
                        private$delims <- delims
                      else
                        stop("Only ASCII-characters (numerical value < 256) accepted.")
                    },
                    getDelimiters = function() {
                      return(private$delims)
                    },
                    
                    nextToken = function() {
                      if (private$nofile) return(NA)
                      if (!CWmisc_validPtr(private$fd$map,private$currentPtr,private$fd$sz)) return(NA)
                      ret<-CWmisc_nextToken(private$currentPtr,private$delims)
                      private$currentPtr<-ret$ptr
                      while ((ret == "") && private$skipEmpty) {
                        ret<-CWmisc_nextToken(private$currentPtr,private$delims)
                        private$currentPtr<-ret$ptr
                      }
                      return(ret$token)
                    },
                    close = function() {
                      if (! private$nofile ) CWmisc_munmap(private$fd)
                      private$nofile <- TRUE
                    },
                    print = function() {
                      if (private$nofile)
                        cat("No file open. Please create a new object for reading.\n")
                      else
                        cat(paste0("File open: ", private$fname, ".\n"))
                    }
                  ), # end public members
                  private = list(
                    fname = NA,         # file name
                    fd = NA,            # file descriptor of opened file
                    currentPtr = NA,    # start of next token
                    nofile = FALSE,     # no file given or file closed
                    delims = NA,        # delimiters, NA is default
                    skipEmpty = TRUE    # skip empty tokens?
                  ) # end private members
                  
)
