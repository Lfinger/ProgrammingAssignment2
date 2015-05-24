## The makeCacheMatrix and cacheSolve functions will mean that repeated time consuming computation
## of the inverse of a matrix can be avoided, with the inverse retrieved from a cache instead.

## makeCacheMatrix will create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function(incomingx) {
    x <<- incomingx
    matrixinverse <<- NULL
  }
  get <- function() {
    x
  }
  setsolve <- function(incomingmatrixinverse) {
    matrixinverse <<-incomingmatrixinverse
  }
  getsolve <- function() {
    matrixinverse
  }
  list(set=set,get=get,setsolve=setsolve, getsolve=getsolve) ##return vector type list of functions
}

## cacheSolve computes the inverse of the makeCacheMatrix object. If the inverse
## has already been calculated and the matrix has not been changed since, then
## cacheSolve will retrieve the inverse from the cache rather than calculating it from scratch.
cacheSolve <- function(x, ...) {
  matrixinverse <- x$getsolve()
  if(is.null(matrixinverse)) {
    data<-x$get()
    matrixinverse<-solve(data,...)
    x$setsolve(matrixinverse)
  }
  else {
    message("getting cached data")
  }
  matrixinverse
}
