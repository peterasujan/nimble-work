library(nimble)
source('importance.R')

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
                         constants = list(SAMPS = length(x)),
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

t = sum(x)
n = length(x)
curve(dbeta(x, 10 + t, 30 + n - t), add = TRUE) ## analytic posterior

(10 + t) /  (40 + n) ## analytic posterior mean
mean(resample) ## empirical posterior mean
