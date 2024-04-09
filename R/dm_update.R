# dirichlet-multinomial posterior is just a dirichlet with 
# posterior params = prior params + counts 

#' @export
dm_update <- function(x, prior_params=rep(1/2, length(x))) {
  if (!is.null(dim(x))) x <- colSums(x)

  return(prior_params + x)
}
