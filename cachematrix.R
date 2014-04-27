## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates the vector invMatrix.
# The value of invMatrix is set.
# The inverse of the matrix x is then computed and affected
# to invMatrix via setInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) invMatrix <<-inverse
  getInverse <- function() invMatrix
  
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


# cacheSolve calculates the inverse of invMatrix created by makecacheMatrix.
# The function checked if the inverse has been calculated already.
# If it's the case, it retrieve the inverse from the cache (via getInverse).
# If not, it computes the inverse of the matrix and sets this matrix in the cache via setInverse.

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  # Test wether an inverse already exists
  if (!is.null(invMatrix)) { 
    message("Getting cached data")
    return(invMatrix)
  } 
  #Calculate the inverse
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
  
  return(invMatrix)
}
