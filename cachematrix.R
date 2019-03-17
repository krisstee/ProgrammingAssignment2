# Author: Kristijana A.
# Programming Assignment 2

# The following functions can be used together to compute and cache the
# inverse of a matrix. makeCacheMatrix creates a special "matrix" that
# can cache its inverse. cacheSolve computes the inverse of the "matrix"
# returned by makeCacheMatrix or returns the cached answer if it was
# previously computed.

# Def: Creates a special "matrix" object can cache its inverse
# Args: x - an invertible matrix; default value is an empty matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Def: Computes the inverse of the special "matrix" returned from the
# makeCacheMatrix function. If the  inverse has already been computed, the
# function returns it from the cache
# Args: x - the special "matrix" returned from makeCacheMatrix
# "..." - further arguments passed to or from other methods
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
