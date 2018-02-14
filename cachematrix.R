## The function makeCacheMatrix creates a matrix and caches the inverse result
## of that matrix.
## If available, the inverse matrix can be looked up in the cache rather 
## then being recomputed again.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  ## set matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  ## get matrix 
  get <- function() x
  
  ## set inverse matrix 
  setInverse <- function(solve) inv <<- solve(x)
  
  ## get inverse matrix   
  getInverse <- function() inv


  ## the input for cashSolve
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The function cacheSolve returns a matrix that is the inverse of matrix 'x' returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix has not changed, then the cacheSolve 
## retrieves the inverse from the cache. Otherwise the function will calculate the inverse matrix of x
## It is assumed that the matrix supplied is always a square invertible matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
 
## Check if the inverse matrix has already been calculated.
## if not,  calculate the inverse matrix and assign result to cache 
  
  if (!is.null(inv))
    {
    message("getting cached data")
    return(inv)
    }
  mat <- x$get()
  inv <- solve(mat, ...)  #calculate the inverse matrix
  x$setInverse(inv)       #assign result to cache
  return(inv)             #print inverse matrix to console
}




