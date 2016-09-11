# Caching the Inverse of a Matrix
#
# Because matrix inversion is a very hard computation, so it has mean to cache
# the inverse of a matrix (instead of compute it again).
#
# These two functions are used to create an object that stores a matrix
# and caches its inverse.
#
# makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve: it tries to search that whether inverse of matrix had been
# computed and if so it returns with the cached value if not it calculates
# the inverse and cache it.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
