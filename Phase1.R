#1
# we are using Linear congruential generator its better not to change the parameters
regnrator <- function(m = 2 ^ 48, a = 25214903917, c = 11, floor = 0,  roof = 1, n = 1, Integer = FALSE){
  
  distance <- roof - floor
  
  #seed(using system time for most randomness):
  seed <- as.numeric(Sys.time()) * 1000
  
  #output numbers:
  numbers <- vector(length = n)
  
  seed <- (a * seed + c) %% m
  for(i in 1:n){
    seed <- (a * seed + c) %% m
    numbers[i] <- (seed / m) * distance + floor
  }
  
  if(Integer)
    for(i in 1:n)
      numbers[i] <- as.integer(numbers[i])
  
  return(numbers)
  
}

#3
cugen <- function(){
  return(dugen(1,0))
}

#6
gegen <- function(parameter){
  
  i <- 0
  result <- brgen(parameter) 
  
  while(result < parameter){
    result <- brgen(parameter)
  }
  
  result <- brgen(parameter)
  
  while(result < parameter){
    i++
      result <- brgen(parameter)
  }
  
  return(i)
  
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