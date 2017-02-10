#' Create an \code{Automat} for stateful computation, such as parsing.
#'
#' @description Sometimes computation has to be stateful, such as in parsing files etc. The aim of the \code{Automat} class is to abstract this away from the user. 
#' 
#' @details An \code{Automat} is a finite state machine (or DFA[1]). For usage it is first set up with a set of transitions. Then it is fed with inputs, triggering internal transitions and returning outputs as determined by the current internal state and the input. Each transition can be augmented by a user-supplied function, that can compute return values or trigger side effects. Support for 'from-any' and 'by-any' transitions is also provided. Explicitly set transitions take precedence over these wildcard transitions, with by-any-transitions taking precedence over from-any-transitions. Finally each state can be augmented with a predicate function that transforms the actual input into a new input that is actually used for selecting the proper transition. Each transition can also be annotated with a function that may generate additional outputs or side effects like maintaining stacks or other memory to implement more powerful classes of automata.
#' 
#' Footnotes:
#' [1]\url{https://en.wikipedia.org/wiki/Deterministic_finite_automaton}
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @import hash 
#' 
#' @section Methods:
#' 
#' \describe{
#'   \item{\code{new()}}{Create a new instance of an \code{Automat}.}
#'   \item{\code{addTransition(from,input,to,FUN)}}{Add a transition \code{from} a named state upon \code{input} \code{to} named state. While any comparable type would do, these values are restricted to character types by the internal implementation. Use \code{as.character()} if your states are simply numbered. The function \code{FUN} is called upon a successful transition with the previous state, the current state and the original input as argument. It is used for generating side effects or producing output values.}
#'   \item{\code{setPredicate(state,PFUN)}}{Used to set a predicate function \code{PFUN} for a given state. A predicate function is a function called on the input before it is processed. It must return a value of type character but can be used to extend the input domain to arbitrary objects and conveniently implement complex transitions (e.g. depending on prefixes).}
#'   \item{\code{setState(state)}}{Set the \code{Automat} to a certain state (e.g. to the initial state).}
#'   \item{\code{read(input)}}{Tell the \code{Automat} to read/consume the given \code{input} and act upon it.}
#'   \item{\code{print(long=F)}}{Prints a summary of the \code{Automat}. When \code{long} is \code{TRUE}, a full list of states and transitions is returned.}
#'   \item{\code{getGraph()}}{Produces a renderable representation of the state graph of the \code{Automat} using the \code{DiagrammeR} package.}
#'   \item{\code{visualize()}}{Displays the state graph of the \code{Automat} using the \code{DiagrammeR} package.}
#' }
#'
# nolint start
#' @usage 
#'# A <- Automat$new()
#'# a$addTransition(from,input,to,FUN)
#'# a$setState(state)
#'# a$setPredicate(state,PFUN)
#'# a$read(input)
#'# a$print(long=F)
#'# a$visualize()
# nolint end 
#' 
#' @param from A character object naming the state this transition starts from. Can be \code{NA} to produce a 'from-any'-transition (e.g. for triggering a reset into a special state).
#' @param to A character object naming the state this transition leads to.
#' @param input The input triggering the state transition. Can be \code{NA} to create a 'on-any'-transition. 
#' @param state A character object defining a state.
#' @param FUN A function taking the current state, the read input and the next state as parameters. The function need not be deterministic and can have side effects.
#' @param PFUN A predicate function for the given stateThe funtion is invoked on the input before it is further processed. It is intended for more complex transitions conditions (e.g. input string has 3 "x"'s) or converting non-character inputs to character strings.
#' @param long when \code{TRUE} a detailed description of the state machine is printed, a short summary otherwise (default behaviour)
#' @return For \code{new()} A new Automat object, nothing for \code{setState()}. The return value of \code{read()} depends on the function associated with the transition triggered. 
#' 
# nolint start 
#' @examples
#'   A <- Automat$new()
#'   f <- function(s,i,t){paste0("You caught ",i,"!")}
#'   g <- function(s,i,t){paste0("You caught the flu!")}
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
#'   A$visualize()}
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
                       private$predicates <- hash()
                       reg.finalizer(self,function(e) {
                         clear(private$byany)
                         clear(private$fromany)
                         clear(private$predicates)
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
                    setPredicate = function(from,FUN){
                      if (!is.character(from)) stop("Not a valid state descriptor.")
                      private$predicates[[from]]<-FUN
                    },
                    setState = function(to){
                      if (!is.character(to)) stop("Not a valid state descriptor.")
                      private$current<-to
                    },
                    read = function(input) {
                      rawinput<-input
                      trans<-NULL
                      curr<-private$current
                      if (is.na(curr)) stop("Not in a valid state. Use setState() to set a start state.")
                      if (!is.null(p<-private$predicates[[curr]])) {
                        input<-p(input)
                      }
                      if (is.hash(s<-private$states[[curr]])){
                        trans<-s[[input]]
                      }
                      # if no explicit transition is given, look for a byany one
                      if (is.null(trans)){
                        trans<-private$byany[[curr]]
                      }
                      # if none is found, look for a fromany one
                      if (is.null(trans)){
                        trans<-private$fromany[[input]]
                      }
                      # last resort is the default action
                      if (is.null(trans)){
                        trans<-private$defaultAction
                      }
                      if (is.null(trans)){
                        stop("No matching transition found for state (",curr,") and input (",input,")")
                      }
                      ret<-NULL
                      if (!is.null(trans$f)){
                        ret<-trans$f(private$current,rawinput,trans$t)
                      }
                      if (!is.null(private$alwaysAction)){
                        private$alwaysAction(private$current,rawinput,trans$t)
                      }
                      private$current<-trans$t
                      if (is.null(ret)) return(invisible(ret))
                      return(ret)
                    },
                    print = function(long=FALSE) {
                      # produce the transition table to catch states which were declared only as target
                      ttable<-private$ttable()
                      n <- unique(c(ttable[,1],ttable[,4]))
                      n <- n[n!="*"]
                      c <- private$current
                      if (is.na(c)) c <- "not set"
                      cat(paste0("An Automat with ",length(n)," states.\n"))
                      cat(paste0("Current state is: ",c))
                      # print the detailed information
                      if (long) {
                          cat("\n\nStates:\n=======\n")
                          cat(paste0(sort(n)))
                          cat("\n\nTransitions:\n============\n")
                          print(ttable)
                          cat("\n(First match in list is used.)\n")
                          if (!is.null(private$alwaysAction)){
                            cat("There is a function that is invoked after each valid transition.")
                          }
                          preds<-sapply(n,function(x) !is.null(private$predicates[[x]]))
                          if (any(preds)){
                            cat("\n\nPredicates for states:\n======================\n")
                            print(n[preds])
                          }
                        }
                    },
                    renderGraph = function() {
                      if (requireNamespace("DiagrammeR", quietly = TRUE)) {
                        ttable<-private$ttable()
                        n <- unique(c(ttable[,1],ttable[,4]))
                        n <- n[n!="*"]
                        #get the nodes_df
                        if (!is.na(private$current)){
                          nn<-n[n!=private$current]
                        } else {
                          nn<-n
                        }
                        if (length(nn)==0) {
                          nodes<-data.frame(
                            id=integer(0),
                            label=character(0),
                            style=character(0),
                            color=character(0),
                            shape=character(0),
                            stringsAsFactors = F)
                        } else {
                          nodes<-DiagrammeR::create_node_df(
                            n = length(nn),
                            label = nn,
                            style = "filled",
                            color = "royalblue2",
                            shape = "circle"
                        )}
                        if (!is.na(private$current)){
                          nodes <- DiagrammeR::combine_ndfs(nodes,DiagrammeR::create_node_df(
                            n = length(private$current),
                            label = private$current,
                            style = "filled",
                            color = "springgreen3",
                            shape = "circle"
                          ))
                        }
                        # create the graph so far
                        if (dim(nodes)[1]>0) {
                          g <- DiagrammeR::create_graph(nodes_df = nodes)
                        } else {
                          g <- DiagrammeR::create_graph()
                        }
                        # produce the edge_df
                        ttable[,3] <- sapply(ttable[,3],FUN=function(x){
                                                              if (x) ", f()" else ""
                                                            })
                        directs <- (ttable[,1]!="*" & ttable[,2]!="*")
                        if (length(directs > 0) & any(directs)){
                          directs <- matrix(ttable[directs,c(1,4,2,3)],ncol=4)
                          for (i in 1:nrow(directs)){
                            if (!edge_present_lab(g,directs[i,1],directs[i,2])){
                            g <- DiagrammeR::add_edge(g,
                                from = directs[i,1],
                                to = directs[i,2],
                                rel = paste0(directs[i,3],
                                             directs[i,4]),
                                use_labels = TRUE) 
                            g <- DiagrammeR::select_last_edge(g)
                            g <- DiagrammeR::set_edge_attrs_ws(g,"color", "#000000")
                            g <- DiagrammeR::set_edge_attrs_ws(g,"fontcolor", "#000000")
                            g <- DiagrammeR::set_edge_attrs_ws(g,"input", directs[i,3])
                            g <- DiagrammeR::set_edge_attrs_ws(g,"hook", directs[i,4])
                            g <- DiagrammeR::set_edge_attrs_ws(g,"label", paste0(directs[i,3],
                                                                            directs[i,4]))
                            g <- DiagrammeR::clear_selection(g)
                            }
                          }
                        }
                        # now add wildcard edges
                        byany <- (ttable[,1]!="*" & ttable[,2]=="*")
                        if (length(byany > 0) & any(byany)){
                          byany <- matrix(ttable[byany,c(1,4,2,3)],ncol=4)
                          for (i in 1:nrow(byany)){
                            if (!edge_present_lab(g,byany[i,1],byany[i,2])){
                              g <- DiagrammeR::add_edge(g,
                                  from = byany[i,1],
                                  to = byany[i,2],
                                  rel = paste0(byany[i,3],
                                               byany[i,4]),
                                  use_labels = TRUE)
                              g <- DiagrammeR::select_last_edge(g)
                              g <- DiagrammeR::set_edge_attrs_ws(g,"color", "#444444")
                              g <- DiagrammeR::set_edge_attrs_ws(g,"fontcolor", "#444444")
                              g <- DiagrammeR::set_edge_attrs_ws(g,"input", byany[i,3])
                              g <- DiagrammeR::set_edge_attrs_ws(g,"hook", byany[i,4])
                              g <- DiagrammeR::set_edge_attrs_ws(g,"label", paste0(byany[i,3],
                                                                              byany[i,4]))
                              g <- DiagrammeR::clear_selection(g)
                            }
                          }
                        }
                        fromany <- (ttable[,1]=="*" & ttable[,2]!="*")
                        if (length(fromany > 0) & any(fromany)){
                          fromany <- matrix(ttable[fromany,c(1,4,2,3)],ncol=4)
                          for (i in 1:nrow(fromany)){
                            for (j in n){
                              if (!edge_present_lab(g,j,fromany[i,2])){
                                g <- DiagrammeR::add_edge(g,
                                    from = j,
                                    to = fromany[i,2],
                                    rel = paste0(fromany[i,3],
                                                 fromany[i,4]),
                                    use_labels = TRUE) 
                                g <- DiagrammeR::select_last_edge(g) 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"color", "#888888") 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"fontcolor", "#888888") 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"input", fromany[i,3]) 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"hook", fromany[i,4]) 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"label", paste0(fromany[i,3],
                                                                                fromany[i,4])) 
                                g <- DiagrammeR::clear_selection(g)
                              }
                            }
                          }
                        }
                        default <- (ttable[,1]=="*" & ttable[,2]=="*")
                        if (length(default > 0) & any(default)){
                        default <- matrix(ttable[default,c(1,4,2,3)],ncol=4)
                          for (i in 1:nrow(default)){
                            for (j in n){
                              if (!edge_present_lab(g,j,default[i,2])){
                                g <- DiagrammeR::add_edge(g,
                                    from = j,
                                    to = default[i,2],
                                    rel = paste0(default[i,3],
                                                 default[i,4]),
                                    use_labels = TRUE) 
                                g <- DiagrammeR::select_last_edge(g) 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"color", "#BBBBBB") 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"fontcolor", "#BBBBBB") 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"input", default[i,3]) 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"hook", default[i,4]) 
                                g <- DiagrammeR::set_edge_attrs_ws(g,"label", paste0(default[i,3],
                                                                                default[i,4])) 
                                g <- DiagrammeR::clear_selection(g)
                              }
                            }
                          }
                        }
                        g <-
                          DiagrammeR::set_global_graph_attrs(g,"layout","dot",attr_type = "graph")
                        return(g)
                      } else {
                        warning("Sorry Dave, I can't do that.\nPlease install DiagrammeR") # nocov
                      }
                    },
                    visualize = function(g=NULL) {
                      if (requireNamespace("DiagrammeR", quietly = TRUE)) {
                        if (is.null(g)) g<-self$renderGraph()
                        DiagrammeR::render_graph(g)
                      } else {
                        warning("Sorry Dave, I can't do that.\nPlease install DiagrammeR") # nocov
                      }
                    }
                  ), # end public members
                  private = list(
                    current = NA,           # the current state
                    states = NA,            # (from-)state-transition maps
                    byany = NA,             # (from-)state-on-any-transitions
                    fromany = NA,           # from-any-transitions by input
                    predicates = NA,        # state-dependent preprocesing of input
                    defaultAction = NULL,   # a default action if no transition pattern matches
                    alwaysAction = NULL,     # a function hook run after every step
                    ttable = function(){
                      ttable <- c()
                      for (s in keys(private$states)){
                        t<-private$states[[s]]
                        for (i in keys(t)) {
                          ttable<-rbind(ttable,c(s,i,!is.null(t[[i]]$f),t[[i]]$t))
                        }
                      }
                      t<-private$fromany
                      for (i in keys(t)) {
                        ttable<-rbind(ttable,c("*",i,!is.null(t[[i]]$f),t[[i]]$t))
                      }
                      t<-private$byany
                      for (s in keys(t)) {
                        ttable<-rbind(ttable,c(s,"*",!is.null(t[[s]]$f),t[[s]]$t))
                      }
                      if (!is.null(private$defaultAction)) {
                        t<-private$defaultAction
                        ttable<-rbind(ttable,c("*","*",!is.null(t$f),t$t))
                      }
                      if (length(ttable)>0) colnames(ttable)<-c("from","on input","function","to")
                      return(ttable)
                    }
                  ) # end private members
)

#@ internal
edge_present_lab<-function(g,from,to){
  cond1 <- paste0("label == \'",from,"\'")
  cond2 <- paste0("label == \'",to,"\'")
  n.from<-DiagrammeR::get_node_ids(g, conditions = cond1)
  n.to<-DiagrammeR::get_node_ids(g, conditions = cond2)
  if (length(from) == 0 | length(to) == 0) {
    return(FALSE)
  } else {
    return(DiagrammeR::edge_present(g,n.from,n.to))
  }
}

#@ internal
requireGraphing<-function(){
  return(requireNamespace("DiagrammeR", quietly = TRUE))
}
