## RPROG Programming Assignment 2
 
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  get <- function() x
  set <- function(y){
    x <<- y;
    inv <<- NULL
  }
  
  getinv <- function() inv
  setinv <- function(ii) inv <<- ii
  return(list(get = get, set = set, getinv = getinv, setinv = setinv))
}


#  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  m <- solve(x$get(), ...)
  x$setinv(m)
  return(m)
}