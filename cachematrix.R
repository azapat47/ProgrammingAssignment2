## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  set.inverse <- function(i) inverse <<- i
  get.inverse <- function() inverse
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## Write a short comment describing this function

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
