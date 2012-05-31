estimatePi <- function(seed)
{
  set.seed(seed)
  numDraws <- 1e6
  r <- 0.5
  x <- runif(numDraws, min=-r, max=r)
  y <- runif(numDraws, min=-r, max=r)
  inCircle <- ifelse((x^2 + y^2)^0.5 < r, 1, 0)
  return(sum(inCircle)/length(inCircle) * 4)
}

id <- function(x) { return(x) }

#parameters
alphas <- c(1.01, 1.05, 1.1, 1.15, 1.2, 1.3, 1.4, 1.5, 1.8, 2.5,3,3.5)
times.sleep <- c(0.01, 0.2, 0.5, 1, 5, 30)
time.total <- 100
seeds <- rnorm(50) + 5 * 500
N <- 10

results <- data.frame()
for(i in 1:N) {
  for(a in alphas) {
    for(time in times.sleep) {
      args <- rep(time, time.total/time)
      tryCatch({
      t <- test(sleep.wrapper, args, a)
      results <- rbind(results, c(a, time, t))
      }, error = function(err) print(err))
    }
  }
}

test <- function(f, args, alpha) {
  t <- system.time({
    jids <- rcloud.map(f, args, alpha = alpha)

    while(!rcloud.finished(jids)) Sys.sleep(1)
  })[3]
  return(t)
}

sleep.wrapper <- function(t) {
  Sys.sleep(t)
}
