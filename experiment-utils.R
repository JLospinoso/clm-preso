#install.packages("ordinal")
library(ordinal)

make.density <- function(param, intercept, var.a, var.b) {
  regression.part <- as.numeric(param %*% rbind(var.a, var.b))
  logit <- sapply(intercept, function(x) x - regression.part)
  cumulative.density <- 1/(1+exp(-logit))
  cumulative.density <- cbind(cumulative.density,1)
  density <- t(apply(cumulative.density, 1, diff))
  density <- cbind(cumulative.density[,1], density)
  density
}

rmulti <- function(p) { which(rmultinom(1, 1, p) == 1) }

sample.data <- function(density, var.a, var.b) {
  data.frame(y=factor(apply(density, 1, rmulti)), x1=var.a, x2=var.b)
}

sample.z.clm <- function(density, var.a, var.b) {
  s <- sample.data(density, var.a, var.b)
  regression <- clm(y~x1+x2, data=s)
  z.values <- regression$coefficients / sqrt(diag(regression$vcov))
  z.values
}

sample.z.lm <- function(density, var.a, var.b) {
  s <- sample.data(density, var.a, var.b)
  regression <- lm(as.numeric(y)~x1+x2, data=s)
  z.values <- regression$coefficients / sqrt(diag(vcov(regression)))
  z.values
}

do.run <-function(coef, sampler) {
  param <- c(0, coef)
  intercept <- c(0,.25,.35,.45)
  observations = 100
  scale.b <- 10
  var.a <- runif(observations, min=0, max=10)
  var.b <- rpois(observations, var.a)
  d <- make.density(param, intercept, var.a, var.b)
  sampler(d, var.a, var.b)
}

est.positives <- function(n, coef, sampler) {
  study <- replicate(n, do.run(coef, sampler), simplify=FALSE)
  beta.z <- sapply(study, function(x)tail(x,2))
  false.pos <- sum(abs(beta.z[1,]) > 1.96) / n
  true.pos <- sum(abs(beta.z[2,]) > 1.96) / n
  r <- c(false.pos, true.pos)
  names(r) <- c("false", "true")
  r
}

set.seed(0)

experiment <- function(coef) {
  draws <- 1000
  clm=est.positives(draws, coef, sample.z.clm)
  lm=est.positives(draws, coef, sample.z.lm)
  cat(".")
  result <- c(coef, clm, lm)
  names(result) <- c("coef", "clm-F", "clm-T", "lm-F", "lm-T")
  result
}

