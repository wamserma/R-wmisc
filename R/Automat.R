#' Create a \code{Automat} for stateful computation, such as parsing.
#'
#' @description Sometimes computation has to be stateful, such as in parsing files etc. The aim of the \code{Automat} class is to abstract this away from the user. 
#' 
#' @details An \code{Automat} is a finite state machine (or [DFA](https://en.wikipedia.org/wiki/Deterministic_finite_automaton)). It is first set up with a set of transitions. Then it is fed with inputs, triggering internal transitions and returning outputs as determined by the current internal state and the input. Each transition can be augmented by a user-supplied function, that can compute return values or trigger side effects. Support for 'from-any' and 'on-any' transitions is also provided. Explicitly set transitions take precedence over these wildcard transitions, with byany-transition taking precedence over fromany-transitions.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @import hash 
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{new()}}{Create a new instance of an \code{Automat}.}
#'   \item{\code{addTransition(from,input,to,FUN)}}{Add a transition 'from' a named state upon 'input' 'to' named state. While any comparable type would do, these values are restricted to character types by the internal implementation. Use 'as.character()' if your states are simply numbered. The function FUN is called with the previous state, the current state and the input as argument. It is used for generating side effects or producing output values.}
#'   \item{\code{setState(to)}}{Set the \code{Automat} to a certain state (e.g. to the initial state).}
#'   \item{\code{read(input)}}{Tell the \code{Automat} to read/consume the given input and act upon it.}
#'   \item{\code{print(long=F)}}{Prints a summary of the \code{Automat}. When long is true, a full list of states and transitions is returned.}
#'   \item{\code{visualize()}}{Prints the state graph of the \code{Automat}.}
#' }
#'
# nolint start
#' @usage 
#'  # A <- Automat$new()
#'  # a$addTransition(from,input,to,FUN)
#'  # a$read(input)
# nolint end 
#' 
#' @param from A character object naming the state this transition starts from. Can be \code{NA} to produce a 'from-any'-transition (e.g. for triggering a reset into a special state).
#' @param to A character object naming the state this transition leads to.
#' @param input The input triggering the state transition. Can be \code{NA} to create a 'on-any'-transition. 
#' @param FUN A function taking the current state, the read input and the next state as parameters. The function need not be deterministic and can have side effects.
#' @return For \code{new()} A new Automat object, nothing for \code{setState()}. The return value of \code{read()} depends on the function associated with the transition triggered. 
#' 
# nolint start 
#' @examples
#'   A<-Automat$new()
#'   f<-function(s,i,t){paste0("You caught ",i,"!")}
#'   g<-function(s,i,t){paste0("You caught the flu!")}
#'   A$addTransition("ready",NA,FUN=f) # add a loop
#'   A$setState("ready")
#'   A$read("Pikachu")
#'   A$read("Bulbasaur")
#'   A$read("Squirtle")
#'   A$addTransition("ready","Pikachu","ready",g) # Pikachu no more
#'   A$read("Pikachu")
#'   A$read("Bulbasaur")
#' 
#' \dontrun{
#' }
# nolint end
#' @rdname Automat
#' @format An \code{\link{R6Class}} generator object. 
#' @export

Automat <- R6::R6Class("Automat",
                  public = list(
                     initialize = function() {
                       private$states <- hash()
                       private$byany <- hash()
                       private$fromany <- hash()
                       reg.finalizer(self,function(e) {
                         clear(private$byany)
                         clear(private$fromany)
                         for (h in names(private$states)) {
                           clear(private$states[[h]])
                         }
                         clear(private$states)
                       },
                       onexit = TRUE)
                    }, # end initialize
                    addTransition = function(from,input,to=from,FUN=NULL) {
                      # wildcard transitions
                      if (is.na(from) & is.na(input)){
                        if (is.na(to)) {
                          private$alwaysAction<-FUN
                        } else {
                          private$defaultAction<-list(t=to,f=FUN)
                        }
                        return(invisible(NULL))
                      }
                      if (is.na(from)){
                        if (is.na(to)) {
                          stop("Invalid transition without target.")
                        } else {
                          private$fromany[[input]]<-list(t=to,f=FUN)
                        }
                        return(invisible(NULL))
                      }
                      if (is.na(input)){
                        if (is.na(to)) {
                          stop("Invalid transition without target.")
                        } else {
                          private$byany[[from]]<-list(t=to,f=FUN)
                        }
                        return(invisible(NULL))
                      }
                      # regular transition
                      if (! has.key(from,private$states)) {
                        private$states[[from]]<-hash()
                      }
                      private$states[[from]][[input]]<-list(t=to,f=FUN)
                    },
                    setState = function(to){
                      if (!is.character(to)) stop("Not a valid state descriptor.")
                      private$current<-to
                    },
                    read = function(input) {
                      trans<-NULL
                      curr<-private$current
                      if (is.hash(s<-private$states[[curr]])){
                        trans<-s[[input]]
                      }
                      # if no explicit transition is given, look for a byany one
                      if (is.null(trans) ){
                        trans<-private$byany[[curr]]
                      }
                      # if none is found, look for a fromany one
                      if (is.null(trans) ){
                        trans<-private$fromany[[input]]
                      }
                      # last resort is the default action
                      if (is.null(trans) ){
                        trans<-private$defaultAction
                      }
                      if (is.null(trans) ){
                        stop("No matching transition found for state (",curr,") and input (",input,")")
                      }
                      ret<-NULL
                      if (! is.null(trans$f) ){
                        ret<-trans$f(private$current,input,trans$t)
                      }
                      private$current<-trans$t
                      if (!is.null(private$alwaysAction)){
                        private$alwaysAction()
                      }
                      return(ret)
                    },
                    print = function() {
                        n <- max(0,private$states$size) + max(0,private$byany$size) # TODO may count states twice
                        c <- private$current
                        if (is.na(c)) c <- "not set"
                        cat(paste0("An Automat with ",n," states.\n"))
                        cat(paste0("Current state is: ",c))
                    },
                    visualize = function() {
                      # TODO: if package installed, visualize otherwise "sorry Dave I can't do that".
                    }
                  ), # end public members
                  private = list(
                    current = NA,         # the current state
                    states = NA,          # (from-)state-transition maps
                    byany = NA,           # (from-)state-on-any-transitions
                    fromany = NA,         # from-any-transitions by input
                    defaultAction = NULL, # a default action if no transition pattern matches
                    alwaysAction = NULL     # a function hook run after every step
                  ) # end private members
)
