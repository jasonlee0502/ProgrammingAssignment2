## This file is used to cache the inverse of any invertible square matrices.
## If the input matrix is remain same since last time its inverse had been 
## calculated, we can return its cache instead of recalculate it again.
## However, if we does change the matrix, we will calculate it again.

## makeCacheMatix(x)
## return a list of getter,setter methods and getinverse, setinverse methods to
## store the cache of inverse matrix from input matrix. The cached inverse is
## invalidated if the input matrix changed.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL ##initiaze inverse value
  
  ##set the input matrix and reset the cached inverse
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  ##return the input matrix
  get <- function() {x} 
  ##set value of inverse matrix
  setinverse <- function(solve) {inv <<- solve} 
  ##return the cached inverse matrix
  getinverse <- function() {inv}  
  ##return list of setter and getter methods
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve(x)
## return the cached inverse of matrix if the inverse had been calculated and
## its input matrix doesn't change. Recalculate the inverse matrix if the inverse
## has not calculated or the input had changed since last invocation. 

cacheSolve <- function(x, ...) 
{
  ## obtain the inverse of input matrix
  inv <- x$getinverse() 
  
  ## return its cached inverse matrix if the inverse had been computed
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  
  ## recompute the inverse if input matrix changed or the inverse has not calculated
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
