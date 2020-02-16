## 'makeCacheMatrix' and 'cacheSolve' are functions that allow caching the inverse of a matrix
##
## Sample usage:
## m <- matrix(1:4, 2, 2)
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  
  get <- function() {
    m
  }
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of the special matrix returned by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  m
}