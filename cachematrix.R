## Create a vector with four functions. These functions 
## allow the inverse of a specific matrix to be taken
## from cache instead of recalculated each time it is needed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverted <- function(inverted_matrix) m <<- inverted_matrix
  get_inverted <- function() m
  list(set = set, get = get,
       set_inverted = set_inverted,
       get_inverted = get_inverted)
}

## Check to see if inverse matrix has already been calculated.
## If it has not, it is calculated now and stored in cache.
## If it has, it's value will be read from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverted()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverted(m)
  m
}