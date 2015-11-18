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
#     for (i in 1:SAMPS) {
#         x[i] ~ dbern(prob = p)
#     }
    p ~ dbeta(alpha, beta)
})

propModel <- nimbleModel(propCode,
                         constants = list(SAMPS = length(x)),
                         inits = list(alpha = 1, beta = 1))


sampler <- importance_sampler(betaBernModel, propModel, c("p"))

sampler$run(50)

## as.matrix(sampler$mvSamps)

CbetaBern <- compileNimble(betaBernModel)
Cprop <- compileNimble(propModel)
Csampler <- compileNimble(sampler, project = betaBernModel)

Csampler$run(10000)
w <- as.numeric(as.matrix(Csampler$weights["weight"]))
s <- as.numeric(as.matrix(Csampler$mvSamps["p"]))
hist(s)
resample <- sample(s, prob = w, replace = TRUE)
hist(resample, freq = FALSE)

t = sum(x)
n = length(x)
curve(dbeta(x, 10 + t, 30 + n - t), add = TRUE) ## analytic posterior

(10 + t) /  (40 + n) ## posterior mean when solving analytically
mean(resample) ## posterior mean from sampling

