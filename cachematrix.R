## These functions are designed to create a matrix and compute
## the inversion of it

## This function creates a cached inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {inv}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Checks if inverse has been computed and returns it

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
