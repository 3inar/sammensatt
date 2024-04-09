## compositional data functions
closure <- function(v) {
  v/sum(v)
}

`%+%` <- function(u, v) {
  closure(u*v)
}

cneg <- function(v) {
  closure(1/v)
}

`%-%` <- function(u,v) {
  u %+% cneg(v)
}

`%.%` <- function(a, v) {
  closure(v^a)
}

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

cnorm <- function(v) {
  sqrt(v %<>% v)
}

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
