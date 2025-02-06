####  Common Resources  ####

## Suite of functions that can be used to define species response to environmental change and eventually used in the `sim_vs_hab_suit` R function.
unif_func<- function(x, min, max){
  out<- dunif(x, min = min, max = max)
  return(out)
}

linear_func<- function(x, a, b){
  out<- a*x + b
  return(out)
}

norm_func<- function(x, mean, sd){
  out<- dnorm(x, mean = mean, sd = sd)
  return(out)
}

quad_func<- function(x, a, b, c){
  out<- a*x^2 + b*x + c
  return(out)
}

custnorm_func<- function(x, mean, diff, prob){
  prob <- prob + (1 - prob)/2
  sd <- -diff/qnorm(p = 1 - prob)
  out<- dnorm(x, mean = mean, sd = sd)
  return(out)
}

logistic_func<- function(x, alpha, beta){
  out<-  1/(1 + exp((x - beta)/alpha))
  return(out)
}

beta_func<- function(x, p1, p2, alpha, gamma){
  if(FALSE){
    x = 8000
    p1 = params_use$args['p1']
    p2 = params_use$args['p2']
    alpha = params_use$args['alpha']
    gamma = params_use$args['gamma']
  }
  k <- 1/((alpha * (p2 - p1)/(alpha + gamma))^alpha)/((gamma * (p2 - p1)/(alpha + gamma))^gamma)
  out<- ifelse(x > p1 & x < p2, k * ((x - p1)^alpha) * (p2 - x)^gamma, 0)
  return(out)
}