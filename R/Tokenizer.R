#' Create a Tokenizer
#'
#' Upon creation the Tokenizer will try to map the given file to memory for fast access.
#' 
#' supported functions:
#' 
#' nextToken, setDelimiters , getDelimiters, close
#'
#' internally: if defaulut delimiter, use optimised comparison function
#'
#' Remember to alaways close a Tokenizer after use!
#'
#' Note that the sequence crlf (TODO: escape backslash) consists of two characters and therefore will give an empty token, when both are used as delimiters. 
#'
#' @param file The file to open.
#' @param tokens A character vector of ASCII-Characters that serve as delimiters. If not set, it defaults to blank, tab, carriage return and linefeed (the last two together resemble a Windows newline).
#' @return A Tokenizer containing information on \code{file}.
#' @examples
#' Tokenizer("token.txt")
#' 
##' @rdname Tokenizer
##' @export

Tokenizer <- R6::R6Class("Tokenizer",
                  public = list(
                     initialize = function(filename = NA) {
                      if (is.na(filename)) {
                        warning("No file given. Tokenizer will always return NA.", immediate. = T)
                        private$nofile <- TRUE
                      }
                      if (! private$nofile) {
                        filename <- normalizePath( as.character(filename) )
                        if (!file.exists(filename)) {
                          warning("No file at file location '", file, "'. Tokenizer will always return NA.")
                          private$nofile <- TRUE
                        }
                      } 
                      if (! private$nofile) { 
                        private$fname <- filename
                        private$fd = CWmisc_mmap(filename)
                        if (is.null(private$fd[1][[1]])) private$nofile <- TRUE
                        else{
                          private$currentPtr <- private$fd$map
                        }
                      }
                      private$delims <- c(9,10,13,32) # tab, lf, cr, blank
					  reg.finalizer(self,function(e) self$close,onexit = TRUE)
                    }, # end initialize
                    setDelimiters = function(delims) {
                      if (all(delims<256))
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
                    fname = NA,      # file name
                    fd = NA,         # file descriptor of opened file
                    currentPtr = NA, # start of next token
                    nofile = FALSE,  # no file given or file closed
                    delims = NA      # delimiters, NA is default
                  ) # end private members
                  
)

#' mmap, open a file // most of this code is taken from Kevin Ushey's Kmisc package (version 0.5.0)
#' returns a vector of (fd,map-address) on *nix and (hFile,hMap,map) on windows

#sourceCpp(code='
#ifdef WIN32         // means WIN64, too
  #undef Realloc
  #undef Free
  #include <windows.h>
  #include <stdio.h>
  #include <tchar.h>
#else
  #include <sys/mman.h>
  #include <sys/stat.h>
  #include <fcntl.h>   // for open()
  #include <unistd.h>  // for close()
#endif

#using namespace Rcpp;
#using namespace std;          


#}'  