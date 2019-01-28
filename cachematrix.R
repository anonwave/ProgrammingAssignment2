##This function can be used to cache the inverse of a matrix.

makeCachematrix <- function(x = matrix()) {
  inv. <- NULL
  set <- function(y) {
    x <<- y
    inv. <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv. <<- inverse
  getinverse <- function() inv.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This functions checks the cache to see if the result is available. If not available,
##it will calculate the inverse of ‘makeCacheMatrix.’ 

cachesolve <- function(x, ...) {
  inv. <- x$getinverse()
  if(!is.null(inv.)) {
    message("getting cached data")
    return(inv.)
  }
  may <- x$get()
  inv. <- solve(may, ...)
  x$setinverse(inv.)
  inv.
}
