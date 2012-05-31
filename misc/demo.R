# Set the access credentials
rcloud.setkey(3763, "6bbd42ae9e5d2d9a0bd35df5efc4e7b126c3ede6", 3601)

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

# Single call to simple function
jid <- rcloud.call(estimatePi, 19900331)
rcloud.info(jid)
rcloud.finished(jid)
pi <- rcloud.result(jid)
pi

# Map call over simple function
seeds <- runif(100) * 500
seeds
jids <- rcloud.map(estimatePi, seeds)
jids

rcloud.finished(jids)
pies <- rcloud.result(jids)
pies
mean(pies)

###################################################
# Now something with more dependencies

## Function to generate the pxp covariance matrix of the required form
constructSigma <- function(p) {
	c <- c(2, 1, seq(length=p-2,from=0,to=0),1, 2, seq(length=p-2, from=0, to=0))
	for(i in 3:p) {
		c <- c(c, seq(length=i-1,from=0,to=0), 1, seq(length=p-i,from=0,to=0))
	}
	Sigma <- matrix(c, p, p)
	Sigma
}

fun <- function(samples) {
  x <- seq(10,200,by=10)

  Rho12 <- c()
  Rhoi12 <- c()

  # For each value of p we are considering perform the required operations
  for(p in x) {
    n <- p + 10 # number of samples to draw
    cat(p,"\n")
    mu <- seq(length=p,from=0,by=0)

    Sigma <- constructSigma(p)

    # now perform the required numbers of samples
    for(k in 1:samples) {
      X <- rmnorm(n=n, mean=mu, varcov=Sigma)
      S <- cov(X) # unbiased estimator of cov matrix from random sample
      Si <- solve(S) # inverse of the estimator matrix

      Rho12 <- c(Rho12, S[1,2]/sqrt(S[1,1]*S[2,2]))
      Rhoi12 <- c(Rhoi12, -Si[1,2]/sqrt(Si[1,1]*Si[2,2]))
    }
  }

  # Convert the vector into a matrix
  Rho12 <- matrix(Rho12, samples, length(x))
  Rhoi12 <- matrix(Rhoi12, samples, length(x))
}

jid <- rcloud.call(fun, 1000, globals = list(constructSigma = constructSigma), packages = "mnormt")
rcloud.info(jid)
Rhoi12 <- rcloud.result(jid)

png("fun.png")
plot(x,colMeans(Rhoi12),ylim=c(0,1),pch=19,cex=.5, main="Estimating Rho of Inverse Matrix",xlab="p",ylab="Rho12")
lines(x,colMeans(Rhoi12)+apply(Rhoi12, 2, sd))
lines(x=x,y=colMeans(Rhoi12)-apply(Rhoi12, 2, sd))
dev.off()
