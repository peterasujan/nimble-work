#' Creates an importance sampler for the specified model and proposal distribution.
#' 
#' @param model An uncompiled or compiled NIMBLE model.  This argument is required.
#' @param propModel An uncompiled or compiled NIMBLE model.  This argument is required.
#' @param target The nodes to be sampled.
#'
#' @author Peter Sujan
#' 
importance_sampler <- nimbleFunction(
    setup = function(model, propModel, target, silent = FALSE) {
        mvSamps <- modelValues(model)
        mvResamps <- modelValues(model)
        weightsSpec <- modelValuesSpec(vars = c('weight', 'normWeight'),
                                       types = c('double', 'double'),
                                       sizes = list(weight = 1, normWeight = 1))
        weights <- modelValues(weightsSpec)
    },

    ## reset is currently unused (might want to allow repeated re-samples without
    ## taking a new proposal sample??)
    run = function(niter = integer(), reset = logical(default=TRUE)) {
        resize(mvSamps, niter)
        resize(mvResamps, niter)
        resize(weights, niter)
        declare(weightsVector, double(1, niter))
        declare(ids, integer(1, niter))
        weightSum <- 0
        for (i in 1:niter) {
            simulate(propModel)
            nimCopy(from = propModel, to = mvSamps, nodes = target, row = 1, rowTo = i, logProb = FALSE)
            nimCopy(from = propModel, to = model, nodes = target, logProb = FALSE)
            currentWeight <- exp(calculate(model) - calculate(propModel))
            weights['weight', i][1] <<- currentWeight
            weightsVector[i] <- currentWeight
            weightSum <- weightSum + currentWeight
        }
        ## normalize weights
        for (i in 1:niter) {
            weights['normWeight', i][1] <<- weights['weight', i][1] / weightSum
        }
        
        ## resample
        rankSample(weightsVector, niter, ids, silent)
        for (i in 1:niter) {
            nimCopy(from = mvSamps, to = mvResamps, nodes = target, row = ids[i], rowTo = i, logProb = FALSE)
        }
    }
)
