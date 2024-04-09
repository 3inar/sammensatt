# dirichlet-multinomial posterior is just a dirichlet with 
# posterior params = prior params + counts 
dm_update <- function(x, prior_params=rep(1/2, length(x))) {
  if (!is.null(dim(x))) x <- colSums(x)

  return(prior_params + x)
}

## this all might go in its own file?? idk
# posterior mean
dm_posterior_mean <- function(params) {
  params/sum(params)
}

# can generate dirichlet from Gamma:
# https://en.wikipedia.org/wiki/Dirichlet_distribution#Random_variate_generation
rdirichlet <- function(n, params) {
 t(replicate(n, closure(sapply(params, \(a) rgamma(1,a,1)))))
}

# posterior predictive simulation for dm
rdm <- function(n, size, params) {
  pars <- rdirchlet(1, params)
  rmultinom(n, size, params)
}
