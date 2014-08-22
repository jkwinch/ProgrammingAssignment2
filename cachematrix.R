## The two functions makeCacheMatrix and CacheSolve together computes 
## the inverse of a matrix using a cache to store the matrix inverse
## that has been computed. 

## This function defines a list of functions set, get, setinv and getinv.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set= set, get = get, setinv = setinv, getinv = getinv)
}


## This function finds the inverse of a matrix. 
## The first time the inverse of a matrix is found, it is cached so that 
## it can be retrieved the next time without recalculation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

