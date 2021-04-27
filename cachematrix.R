## Cached Object for Inverse Matrix
## Assignment 2 - R Programming Course

## CacheMatrix Object with getter and setter for
## inverse Matrix (prevents unecessary recalculations)
makeCacheMatrix <- function(x = matrix()) {
  # Initializes the Inverse Matrix before calculations
  inverse <- NULL
  
  # Setter for matrix X
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Getter for Matrix X
  get <- function() x
  # Setter and Getter for the Inverse
  set.inverse <- function(i) inverse <<- i
  get.inverse <- function() inverse
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

# Calculates Inverse Matrix only if it was
# not calculated previously and stores it
# as part of the object
cacheSolve <- function(x, ...) {
  inverse <- x$get.inverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  } else {
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$set.inverse(inverse)
    return(inverse)
  }
}
