## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function  returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  ## x is  a square invertible matrix
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## returns the inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix()
  inv = x$getinv()
  
  ## if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skip the computation. 
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise, calculate the inverse 
  mat = x$get()
  inv = solve(mat, ...)
  
  ## sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
