## This script contains two functions: makeCacheMatrix and cacheSolve.
## The goal for this script is to cache the calculation of solve on a matrix at the parent environment so the next time solve's 
## called on the same object it can simply access the cached result instead of running the calculation again.

## makeCacheMatrix will take in a matrix and build the special vector that contains the set and get functions that would allow
## caller function to access the cache if it exists in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  
  set <- function(y){
    x <<- y
    im <<- NULL
    
  }
  get <- function()x
  setSolve <- function(Solve) im <<- Solve
  getSolve <- function() im
  list(set = set, get = get, 
       setSolve= setSolve,
       getSolve = getSolve)
}


## cacheSolve will take in an output from makeCacheMatrix, and it would attempt to access the cached calculation.
## If the cached calculation is not present, then it would run the actual solve() on the matrix and store the 
## result into makeCacheMatrix vector for cache access next time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getSolve()
    if(!is.null(im)){
      message("getting cached data")
      return(im)
    }
    data <- x$get()
    im <- solve(data)
    x$setSolve(im)
    im
}
