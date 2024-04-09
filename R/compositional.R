## compositional data functions

#' @export
closure <- function(v) {
  v/sum(v)
}

#' @export
`%+%` <- function(u, v) {
  closure(u*v)
}

#' @export
cneg <- function(v) {
  closure(1/v)
}

#' @export
`%-%` <- function(u,v) {
  u %+% cneg(v)
}

#' @export
`%.%` <- function(a, v) {
  closure(v^a)
}

#' @export
`%<>%` <- function(u, v) {
  D <- length(u)
  ip = 0
  for (i in 1:D) {
    for (j in 1:D) {
      ip = ip + log(u[i]/u[j])*log(v[i]/v[j])
    }
  }
  
  ip/(2*D)
}

#' @export
cnorm <- function(v) {
  sqrt(v %<>% v)
}

#' @export
variation_matrix <- function(yy) {
  N <- ncol(yy)
  vmat <- matrix(nrow=N, ncol=N)

  for (i in 1:N) {
    for (j in 1:N) {
      vmat[i, j] = var(log(yy[,i]/yy[,j]))
    }
  }

  vmat
} 
