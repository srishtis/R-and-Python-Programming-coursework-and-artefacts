# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## A pair of functions that cache the inverse of a matrix

## Caching the code for Matrix Inversion:

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## Method to get the inverse of the matrix
  getInverse <- function() inv
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  # If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Get the matrix from our object
  mat <- x$get()
  ## Calculate the inverse using matrix multiplication
  inv <- solve(mat, ...)
  ## Set the inverse to the object
  x$setInverse(inv)
  ## Return the matrix
  inv
}









B <- matrix(c(1,2,3,4),2,2)


B1 <- makeCacheMatrix(B)
cacheSolve(B1) #inverse returned after computation

cacheSolve(B1) #inverse returned from cache
