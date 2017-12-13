library(ggplot2)

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
#2
dugen <- function(a,b){
  s = min(a,b)
  s2 = max(a,b)
  return(regnrator(n = 1,floor = s,roof = s2))
}
#3
cugen <- function(){
  return(dugen(1,0))
}
#4
brgen <- function(p)
{
  temp <- cugen()
  if(temp < p)
    return(0)
  else
    return(1)
}
#5
bigen <- function(p, n)
{
  temp <- 1
  for(i in 1:n)
    temp <- temp + brgen(p)
  temp <- temp - 1
  return(temp)
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
    i = i+1
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

#8
gagen <- function(exp_parameter, k){
  sum <- 0
  for(i in 1:k){
    sum <- sum + expgen(exp_parameter)
  }
  return(sum)
}

# 9
pogen <- function(ex,t = 1){
  i = 0;
  sum = expgen(ex)
  while(sum < t)
  {
    sum = sum + expgen(ex)
    i = i + 1 
  }
  return(i)
}


#10
nogen <- function(u, s){
  pois <- pogen(s) #pois = poisson(sigma) ~ N(sigma, sigma)
  normal <- pois - s + u #normal = N(u, sigma) as CLT does
  return(normal)
}

#11

#part 1
f1 <- function(){
  # numbers = 100 , between 0 to 1
  hist(regnrator(n=100), ylab = "count", col = colors())
  # numbers = 1000 , 1 to 10
  hist(regnrator(n=1000, floor = 1, roof = 10), ylab = "count", col = colors())
  # numbers = 1000 , 1 to 10 , Integer = TRUE
  qplot(regnrator(n=1000, floor = 1, roof = 10, Integer = TRUE), ylab = "count")
}

#part 2

f2 <- function(a,b){
  s = 1
  c = c(dugen(a,b))
  k = dugen(a,b)
  while (s < 1000){
    while (k == c[s]){
      k = dugen(a,b)
    }
    c = c(c,k)
    s = s + 1
  }
  uniform = c
  hist(uniform, ylab = "count", col = colors())
}
f2(1,4)
#part 3
f3 <- function(){
  s = 1
  c = c(cugen())
  k = cugen()
  while (s < 1000){
    while (k == c[s]){
      k = cugen()
    }
    c = c(c,k)
    s = s + 1
  }
  uniform_between_zero_and_one = c
  hist(uniform_between_zero_and_one, ylab = "count", col = colors())
}
f3()

#part 4
f4 <- function(a){
  s = 1
  c = c(brgen(a))
  while (s < 1000){
    c = c(c,brgen(a))
    s = s + 1
  }
  qplot(c, ylab = "count")
}
f4(0.5)
#part 5
f5 <- function(a,b){
  s = 1
  c = c(bigen(a,b))
  while (s < 1000){
    c = c(c,bigen(a,b))
    s = s + 1
  }
  Binomial = c
  hist(Binomial, ylab = "count", col = colors())
}
f5(0.5,3)

#part 6
f6 <- function(a){
  s = 1
  c = c(gegen(a))
  while (s < 1000){
    c = c(c,gegen(a))
    s = s + 1
  }
  Geometric = c
  qplot(Geometric, ylab = "count")
}
f6(0.5)
#part 7
f7 <- function(a){
  s = 1
  c = c(expgen(a))
  k = expgen(a)
  while (s < 1000){
    while (k == c[s]){
      k = expgen(a)
    }
    c = c(c,k)
    s = s + 1
  }
  hist(c, ylab = "count", col = colors())
}
f7(3)
#part 8
f8 <- function(a,b){
  s = 1
  c = c(gagen(a,b))
  k = gagen(a,b)
  while (s < 1000){
    while (k == c[s]){
      k = gagen(a,b)
    }
    c = c(c,k)
    s = s + 1
  }
  bins <- seq(floor(min(c)),ceiling(max(c)),0.5)
  gamma = c
  qplot(gamma, breaks = bins, ylab = "count")
}
f8(1,5)
#part 9
f9 <- function(a,b){
  s = 1
  c = c(pogen(a,b))
  k = pogen(a,b)
  while (s < 1000){
    while (k == c[s]){
      k = pogen(a,b)
    }
    c = c(c,k)
    s = s + 1
  }
  Poisson = c
  bins = seq(0,100,1)
  qplot(Poisson, breaks = bins, ylab = "count",xlim = c(0,100))
}
f9(1,10)
# part 10
f10 <- function(a,b){
  s = 1
  c = c(nogen(a,b))
  k = nogen(a,b)
  while (s < 1000){
    while (k == c[s]){
      k = nogen(a,b)
    }
    c = c(c,k)
    s = s + 1
  }
  Normal = c
  bins = seq(-100,100,1)
  qplot(Normal, breaks = bins, ylab = "count",xlim = c(-100,100))
}
f10(0,5)