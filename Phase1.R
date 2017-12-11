#3
cugen <- function(){
  return(dugen(1,0))
}

#7
expgen <- function(lambda){
  uni <- cugen()
  exp <- (-1.0/lambda)*log(uni)
  return(exp)
}