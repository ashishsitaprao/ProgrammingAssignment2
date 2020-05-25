##makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invr <<- inverse
  getInverse <- function() invr
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##computes the inverse of the special "matrix".
##If the inverse has already been calculated, then it will retrieve it from the cache.

cacheSolve <- function(x, ...) {
  invr <- x$getInverse()
  if (!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  mat <- x$get()
  invr <- solve(mat, ...)
  x$setInverse(invr)
  invr
}
