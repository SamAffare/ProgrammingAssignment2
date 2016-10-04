## Inverse Matrix Caching:
## To save time and computer resources it is beneficial to to cahce the inverse of matrix
## rather than compute it repeatedly.
## makeCacheMatrix is a function that creates a matrix object that can cachce its inverse.
##cacheSolve function computes the inverse of the matrix created by makeCacheMatrix if the matrix has changed,
##otherwise it retrieves the inverse from cache.



## makeCacheMatrix is a function that creates a matrix object that can cachce its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
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


##cacheSolve function computes the inverse of the matrix created by makeCacheMatrix if the matrix has changed,
##otherwise it retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
