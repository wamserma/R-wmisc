context("Automat")

test_that("printing empty automat works",{
  A<-Automat$new()
  expect_output(A$print(),"An Automat with 0 states.\nCurrent state is: not set")
})

# create a basic automat for tests
createBasicAutomat<-function(){
  A<-Automat$new()
  A$addTransition("ready","s","steady")
  A$addTransition("steady","g","go")
  A$addTransition("steady","a","ready",FUN=function(a,b,c){
    print("aborted")
  })
  A$addTransition("go","r","ready")
  A$setState("ready")
  return(A)
}

cleanDynamicGraphData<-function(g){
  g$graph_log<-NULL
  g$graph_info<-NULL
  return(g)
}

test_that("printing and running a simple automat works",{
  A<-createBasicAutomat()
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: ready")
  expect_null(A$read("s"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: steady")
  expect_null(A$read("g"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: go")
  expect_null(A$read("r"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: ready")
  expect_null(A$read("s"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: steady")
  expect_output(A$read("a"),"aborted")
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: ready")
  # below I use  . for '(',')' because of regexp matching
  expect_error(A$read("g"),"No matching transition found for state .ready. and input .g.")
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: ready")
  expect_null(A$read("s"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: steady")
})

test_that("always action works",{
  A<-createBasicAutomat()
  A$addTransition(NA,NA,NA,FUN=function(a,b,c){
    cat(paste0("automat went from (",a,") to (",c,") at input (",b,")"))
    })
  expect_output(A$read("s"),"automat went from .ready. to .steady. at input .s.")
  expect_output(A$read("g"),"automat went from .steady. to .go. at input .g.")
  expect_output(A$read("r"),"automat went from .go. to .ready. at input .r.")
})

test_that("default action works",{
  A<-createBasicAutomat()
  A$addTransition(NA,NA,"ready",FUN=function(a,b,c){
    cat(paste0("automat went from (",a,") to (",c,") at input (",b,")"))
  })
  expect_silent(A$read("s"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: steady")
  expect_output(A$read("foo"),"automat went from .steady. to .ready. at input .foo.")
  expect_silent(A$read("s"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: steady")
  expect_silent(A$read("g"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: go")
  expect_output(A$read("bar"),"automat went from .go. to .ready. at input .bar.")
})

test_that("from-any transitions works",{
  A<-createBasicAutomat()
  A$addTransition(NA,"reset","ready",FUN=function(a,b,c){
    cat(paste0("automat went from (",a,") to (",c,") at input (",b,")"))
  })
  expect_silent(A$read("s"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: steady")
  expect_output(A$read("reset"),"automat went from .steady. to .ready. at input .reset.")
  expect_silent(A$read("s"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: steady")
  expect_silent(A$read("g"))
  expect_output(A$print(),"An Automat with 3 states.\nCurrent state is: go")
  expect_output(A$read("reset"),"automat went from .go. to .ready. at input .reset.")
})

test_that("predicates are used",{
  A<-createBasicAutomat()
  A$addTransition(NA,"reset","ready",FUN=function(a,b,c){
    cat(paste0("automat went from (",a,") to (",c,") at input (",b,")"))
  })
  A$addTransition(NA,"gnu","gnat")
  A$setPredicate("gnat",function(i) "reset")
  expect_null(A$read("s"))
  expect_output(A$print(),"An Automat with 4 states.\nCurrent state is: steady")
  expect_null(A$read("gnu"))
  expect_output(A$print(),"An Automat with 4 states.\nCurrent state is: gnat")
  expect_output(A$read(3L),"automat went from .gnat. to .ready. at input .3.")
})


test_that("inputs without from or to fail",{
  A<-Automat$new()
  expect_error(A$addTransition(NA,"foo",NA),"Invalid transition without target.")
})

test_that("inputs without input or to fail",{
  A<-Automat$new()
  expect_error(A$addTransition("foo",NA,NA),"Invalid transition without target.")
})

test_that("long format printing works",{
  A<-createBasicAutomat()
  A$addTransition(NA,"reset","ready",FUN=function(a,b,c){
    paste0("automat went from (",a,") to (",c,") at input (",b,")")
  })
  A$addTransition(NA,"gnu","gnat")
  A$addTransition(NA,NA,"gnu",FUN=function(a,b,c){
    paste0("automat went from (",a,") to (",c,") at input (",b,")")
  })
  expect_output_file(A$print(long=T),"automat-longprint-1.txt",update=F)
  A$addTransition(NA,NA,NA,FUN=function(a,b,c){
    paste0("automat went from (",a,") to (",c,") at input (",b,")")
  })
  expect_output_file(A$print(long=T),"automat-longprint-2.txt",update=F)
  A$setPredicate("gnat",function() "reset")
  expect_output_file(A$print(long=T),"automat-longprint-3.txt",update=F)
  A$addTransition("gnat",NA,"gnu",FUN=function(a,b,c){
    paste0("automat went from (",a,") to (",c,") at input (",b,")") # this transition will never be matched
  })
  expect_output_file(A$print(long=T),"automat-longprint-4.txt",update=F)
})

test_that("visualization in general works",{
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    A<-createBasicAutomat()
    g<-A$renderGraph()
    expect_equal(A$visualize(),A$visualize(g))
  }
})

test_that("visualization of empty automat works",{
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    A<-Automat$new()
    expect_equal_to_reference(cleanDynamicGraphData(A$renderGraph()),"automat-visual-0.rds")
  }
})

test_that("visualization without set state works",{
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    A<-Automat$new()
    A$addTransition("ready","ok","ready")
    expect_equal_to_reference(cleanDynamicGraphData(A$renderGraph()),"automat-visual-nostate.rds")
  }
})

test_that("visualization with implicit state works",{
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    A<-Automat$new()
    A$addTransition("ready","ok","ready")
    A$setState("foo")
    expect_equal_to_reference(cleanDynamicGraphData(A$renderGraph()),"automat-visual-implicit.rds")
  }
})

test_that("visualization works",{
  if (requireNamespace("DiagrammeR", quietly = TRUE)) {
    A<-createBasicAutomat()
    A$addTransition(NA,"reset","ready",FUN=function(a,b,c){
      paste0("automat went from (",a,") to (",c,") at input (",b,")")
    })
    A$addTransition(NA,"gnu","gnat")
    A$addTransition(NA,NA,"gnu",FUN=function(a,b,c){
      paste0("automat went from (",a,") to (",c,") at input (",b,")")
    })
    expect_equal_to_reference(cleanDynamicGraphData(A$renderGraph()),"automat-visual-1.rds")
    A$addTransition(NA,NA,NA,FUN=function(a,b,c){
      paste0("automat went from (",a,") to (",c,") at input (",b,")")
    })
    expect_equal_to_reference(cleanDynamicGraphData(A$renderGraph()),"automat-visual-2.rds")
    A$setPredicate("gnat",function() "reset")
    expect_equal_to_reference(cleanDynamicGraphData(A$renderGraph()),"automat-visual-3.rds")
    A$addTransition("gnat",NA,"gnu",FUN=function(a,b,c){
      paste0("automat went from (",a,") to (",c,") at input (",b,")") # this transition will never be matched
    })
    expect_equal_to_reference(cleanDynamicGraphData(A$renderGraph()),"automat-visual-4.rds")
  }
})

test_that("Pokexample works",{
  A<-Automat$new()
  f<-function(s,i,t){paste0("You caught ",i,"!")} # nolint
  g<-function(s,i,t){paste0("You caught the flu!")} # nolint
  A$addTransition("ready",NA,FUN=f) # add a loop
  A$setState("ready")
  expect_equal(A$read("Pikachu"),"You caught Pikachu!")
  expect_equal(A$read("Bulbasaur"),"You caught Bulbasaur!")
  expect_equal(A$read("Squirtle"),"You caught Squirtle!")
  A$addTransition("ready","Pikachu","ready",g) # Pikachu no more
  expect_equal(A$read("Pikachu"),"You caught the flu!")
  expect_equal(A$read("Bulbasaur"),"You caught Bulbasaur!")
})
