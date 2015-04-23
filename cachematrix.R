## Cached Inverse Matrix

## Functions:
##    makeCacheMatrix
##    cacheColve

## Example usage:
##    cm <- makeCacheMatrix()
##    cm$set(matrix(1:4, 2, 2))
##    cacheSolve(cm) # this is calculated
##    cacheSolve(cm) # this is from cache

## Function makes "special" matrix which will be used when
## calculating inverse matrix by using cached set
makeCacheMatrix <- function(x = matrix()) {
  # matrix variable initialization
  m <- NULL
  
  # setter function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # getter function
  get <- function() x
  
  # the functions are used by caching funtion
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  # list of all sub functions (methods) included this function 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function calculates inverse matrix by using solve() function
## and caches the result for using later use
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  ## if already cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## otherwise calculate
  data <- x$get()
  m <- solve(data, ...)
  
  ## store to cache
  x$setsolve(m)
  m
}
