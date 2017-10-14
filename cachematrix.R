## Put comments here that give an overall description of what your
## functions do

## Function for getting and setting a matrix values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(current_inv) inv <<- current_inv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse of a matrix checking first if the inverse has already
## been calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


## Lets try the functions

set.seed(123)
X <- matrix(rbinom(9, 10, 0.5), nrow = 3, ncol = 3)

cache <- makeCacheMatrix(X)
cacheSolve(cache)  # first time: no cache
cacheSolve(cache)  # second time: with cache
