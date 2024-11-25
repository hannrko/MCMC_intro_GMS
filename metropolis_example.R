p_1 <- function(x) {
  ifelse(x >= 0 & x <= 6, x^3 -6*x^2 + 4*x + 20,0)
}

p_2 <-  function(x){
  ifelse(x >= -2 & x <= 3, x^4 +2*x^2 + 10,0)
}

lnp_denovo <- function(x){
  d <- c(42, 49, 40, 36, 50, 37, 39, 43, 54, 55)
  if (x >= 0){
    p <- x^sum(d)*exp(-length(d)*x)*exp(-(x-70)^2/(2*25))
  }else{p <- 0}
  return(log(p))
}

metro_step <- function(x, p, std_dev=0.5, is_log=False){
  # generate x*
  x_n <- rnorm(n = 1, mean = x, sd = std_dev)
  # sample from uniform dist
  u <- runif(1)
  print(is_log)
  # reject or accept
  if(is_log){
    p_r <- p(x_n) - p(x)
    u <- log(u)
    }else{p_r <- p(x_n)/p(x)}
  # return new or old x
  if(u < p_r){
    return(x_n)
  } else{
    return(x)}
}

metropolis <- function(ns, x0, p, sd, is_log=False){
  xs = rep(0, ns)
  xs[1] = x0
  for(i in 2:ns){
    xs[i] <- metro_step(xs[i-1], p, std_dev=sd, is_log=is_log)
  }
  return(xs)
}

nStep <- 100000

xs1 <- metropolis(nStep, 5, p_1)
xs2 <- metropolis(nStep, 1, p_2)

hist(xs2, 50, freq=FALSE, main="", ylim=c(0, 1), las=1,
     xlab="x", ylab="Probability density")

xs_dn <- metropolis(nStep, 70, p_denovo, 1, is_log=True)
