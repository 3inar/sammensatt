

## this all might go in its own file?? idk
# posterior mean

#' @export
dm_posterior_mean <- function(params) {
  params/sum(params)
}

# can generate dirichlet from Gamma:
# https://en.wikipedia.org/wiki/Dirichlet_distribution#Random_variate_generation

#' @export
rdirichlet <- function(n, params) {
 t(replicate(n, closure(sapply(params, \(a) rgamma(1,a,1)))))
}

# posterior predictive simulation for dm

#' @export
rdm <- function(n, size, params) {
  pars <- rdirchlet(1, params)
  rmultinom(n, size, params)
}
