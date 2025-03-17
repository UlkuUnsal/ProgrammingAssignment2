## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when setting a new matrix
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If the inverse is already calculated, return it from cache
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv  # Return the computed inverse
}
