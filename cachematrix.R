## This script contains functions that work together to cache the results of
## time-consuming matrix-inversion calculations

## This function creates a list that holds four separate functions for storing cached
## calculations and retrieving them. The input is a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This functions checks to see whether there is already a cached calculation
## for the inverse of this particular matrix. If it already exists, the 
## fuction returns the cached inverse matrix. If not, it calculates the inverse
## matrix using the solve() function

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}