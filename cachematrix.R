## The functions take a matrix and creates the inverse that it saves in Cache

## Takes an invertible matrix and returns a list of functions that place the
## inverted matrix into Cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Takes the out list of functions from makeCacheMatrix, checks if if the value
## exists and calculates the inverse of the matrix placing it into cache and
## returning the inverted matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
