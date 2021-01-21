## The first function takes a matrix as an argument and returns an object
## with accessors & mutators for both the matrix and its inverse,
## effectively creating a cache.

## The second function computes the inverse of the matrix returned by the first
## function. If the result is already computed, it will fetch from the cache;
## if not it will calculate it and save it to the cache.

## create a special matrix object with accessors and mutators
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## fetch or solve for the inverse of the matrix from makeCacheMatrix
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  # we assume the matrix is always inversible
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
