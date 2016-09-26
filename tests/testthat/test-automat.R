test_that("printing empty automat works",{
  A<-Automat$new()
  expect_output(A$print(),"An Automat with 0 states.\nCurrent state is: not set")
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

# test printing #nolint
# test long printing #nolint
# test visualization #nolint
