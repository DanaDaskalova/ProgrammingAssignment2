makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function(y = matrix()) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  invertm <- function(solve) matrixinverse <<- solve
  getinvert <- function() matrixinverse
  list(set = set, get = get,
       invertm = invertm,
       getinvert = getinvert)
}
cacheSolve <- function(x, ...) {
  matrixinverse <- x$getinvert()
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$invertm(matrixinverse)
  matrixinverse
}
