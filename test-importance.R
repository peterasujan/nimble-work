library(nimble)
library(testthat)
source('importance.R')

context("importance sampling")

betaBernCode <- nimbleCode({
    for (i in 1:SAMPS) {
        x[i] ~ dbern(prob = p)
    }
    p ~ dbeta(alpha, beta)
})

x <- rbinom(n = 100, size = 1, prob = .4)

betaBernModel <- nimbleModel(betaBernCode,
                             constants = list(SAMPS = length(x)),
                             data = list(x = x),
                             inits = list(alpha = 10, beta = 30))

propCode <- nimbleCode({
    p ~ dbeta(alpha, beta)
})

propModel <- nimbleModel(propCode,
                         inits = list(alpha = 1, beta = 1))


sampler <- importance_sampler(betaBernModel, propModel, c("p"))

CbetaBern <- compileNimble(betaBernModel)
Cprop <- compileNimble(propModel)
Csampler <- compileNimble(sampler, project = betaBernModel)

Csampler$run(10000)
w <- as.numeric(Csampler$weights["weight"])
s <- as.numeric(Csampler$mvSamps["p"])
hist(s)
## resample <- sample(s, prob = w, replace = TRUE)
resample <- as.numeric(Csampler$mvResamps["p"])
hist(resample, freq = FALSE)

t <- sum(x)
n <- length(x)
postAlpha <- 10 + t
postBeta <- 30 + n - t
postSample <- rbeta(10000, shape1 = postAlpha, shape2 = postBeta)
curve(dbeta(x, postAlpha, postBeta), add = TRUE) ## analytic posterior


postAlpha / (postAlpha + postBeta) ## analytic posterior mean
mean(resample) ## empirical posterior mean

test_that("means are equal",
          expect_equal(mean(resample), postAlpha / (postAlpha + postBeta),
                       tolerance = 0.01))

test_that("variances are equal",
          expect_equal(var(resample),
                       postAlpha * postBeta /
                           ((postAlpha + postBeta)^2 * (postAlpha + postBeta + 1)),
                       tolerance = 0.01))

## is testing quantiles overkill??
resampQuantiles <- as.numeric(quantile(resample, probs = 1:99 / 100))
## quantilesCompare <- quantile(postSample, probs = 1:99 / 100)
quantilesCompare <- qbeta(p = 1:99 / 100, shape1 = postAlpha, shape2 = postBeta)

test_that("quantiles of sample match those of analytic posterior", {
    expect_equal(resampQuantiles, quantilesCompare, tolerance = 0.01)
})
