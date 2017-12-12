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

#10
nogen <- function(u, s){
  pois <- pogen(s) #pois = poisson(sigma) ~ N(sigma, sigma)
  normal <- pois - sigma + u #normal = N(u, sigma) as CLT does
  return(normal)
}