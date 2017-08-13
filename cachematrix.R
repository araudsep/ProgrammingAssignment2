## This has 2 functions that together allow caching the inverse
## of a matrix to save computation time. makeCacheMatrix constructs
## the cache with functions to operate it and cacheSolve solves
## square matrixes by querying the cache first and if the value is
## not cached, does so after solving it.

## makeCacheMatrix - this is a so-called "constructor" function that 
## creates 4 functions that have the matrix input "x" as their default 
## arguments. The functions allow setting and querying both the input matrix 
## and it's inverse. If the inverse has been solved once, then it caches the 
## value to save computation time.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
   x <<- y
   inv <<- NULL
   ## note that changing the input matrix empties the cached inverse.
 }
 get <- function() x
 setinv <- function(solve) inv <<- solve
 getinv <- function() inv
 list (set = set, get = get, setinv = setinv, getinv = getinv)
 }
  
## (2) cacheSolve - this checks whether there is a cached value for the
## matrix input "x" using the "getinv" function constructed by "makeCacheMatrix".
## If there is, it returns it. If there isn't then it solves the matrix itself 
## and caches the inverse using the "setinv" function constructed by "makeCacheMatrix".

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
    ## note that this returns the value immediately and stops the function.
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}