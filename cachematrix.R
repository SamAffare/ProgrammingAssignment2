## Inverse Matrix Caching:
## To save time and computer resources it is beneficial to to cahce the inverse of matrix
## rather than compute it repeatedly.
## makeCacheMatrix is a function that creates a matrix object that can cachce its inverse.
##cacheSolve function computes the inverse of the matrix created by makeCacheMatrix if the matrix has changed,
##otherwise it retrieves the inverse from cache.



## makeCacheMatrix is a function that creates a matrix object that can cachce its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # this is the variable to save the cache value
  
  #function to store matrix
  set <- function(y) {
    x <<- y
     inv <<- NULL #clear cache to assign new value
  }
  
  #function to return the stored matrix
  get <- function() x
    
    #funciton to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  #function to retrieve the value in cache  
  getInverse <- function() inv
  
  #returning list of each element
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
  
  #checking to see if a cached value exists to return it
  if (!is.null(inv)) {
    message("Retrieving cached data")
    return(inv)
  }
  # otherwise compute the inverse and store in cache
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv #return inverse
}
