## makeCacheMatrix is a function to create a special matrix object.
## It takes a matrix as argument and has 4 functions.
## get: Get the matrix value
## Set: Set the matrix value
## SetInverse: Calculate inverse of the matrix and save it in cache
## GetInverse: Return the inverse of the matrix from the cache
## The function cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve 
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## The function cacheSolve calculate the inverse of a special matrix  created with
## the makeCacheMatrix function.
## If the inverse has already been computed, it returns the value from the cache.
## It will compute the inverse only if the matrix has been updated.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}