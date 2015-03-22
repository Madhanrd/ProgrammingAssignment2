## The makeCacheMatrix function creates a special "matrix"

## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(special = matrix()) {
  inv <- NULL
  ## 1. set the value of the matrix
  set <- function(y) {
    special <<- y
    inv <<- NULL
  }
  ## 2. get the value of the matrix
  get <- function() special
  ## 3. set the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  ## 4. get the value of the inverse of the matrix
  getinverse <- function() inv
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.

cacheSolve <- function(mat, ...) {
  
  ## Initially we need to run makeCacheMatrix() function to utilize this function
  
  ## It checks if inverse of vector has already been calculated.
  ## If so, it gets the inverse value from the cache and skips the current computation.
  ## Also it gives message to users like "getting cached data"
  inverse <- mat$getinverse()
  if(!is.null(inverse)) {
    message("getting data from cache")
    return(inverse)
  }
  
  ##Else, it calculates the inverse of the matrix.
  ##And sets the value of the inverse in the cache via the setinverse function for future computation.
  
  data <- mat$get()
  inverse <- solve(data, ...)
  mat$setinverse(inverse)
  message("setting data to cache")
  inverse    ## Return a matrix that is the inverse of special matrix
}