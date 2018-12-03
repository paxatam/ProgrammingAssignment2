## functions for saving time while calculating inverse matrices

## Creates special matrix containing inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv__Matrix) {inv <<- inv__Matrix}
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

## returns invers matrix of special matrix

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinv(inv)
  
  inv
}