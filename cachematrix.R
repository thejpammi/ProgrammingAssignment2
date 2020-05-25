## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix prepares vector of functions used to hold cache of inverse matrix and getter/setter methods.

makeCacheMatrix <- function(x = matrix()) {
  r <- length(x[1,])
  c <- length(tm[,1])
  
  if(r != c) {
    message("this is not a square matrix")
    return
  }

  m <- NULL
  setM <- function(y) {
    x <<- y
    m <<- NULL
  }
  getM <- function() x
  getInverse <- function() m
  setInverse <- function(inverse) m <<- inverse
  list(setM = setM, getM = getM, getInverse = getInverse, setInverse = setInverse)

}


## Write a short comment describing this function
## the input is the value returned vector of functions from makeCacheMatrix.
## 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse...")
    return (inverse)
  }
  
  data <- x$getM()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
