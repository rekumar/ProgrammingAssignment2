## 

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