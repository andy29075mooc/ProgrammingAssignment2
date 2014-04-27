## makeCacheMatrix()
## Purpose: This function creates a CacheMatrix object that can store/return
##          a matrix and its inverse
## input: a matrix object
## output: list of 4 functions
##          (1) set() - assigns new matrix
##          (2) get() - returns stored matrix
##          (3) setinv() - assigns new matrix inverse
##          (4) getinv() - returns stored matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve()
## Purpose: This function returns a cached matrix inverse from a CacheMatrix
##          object, or calculates it if not present
## input: a CacheMatrix object (see above function)
## output: inverse matrix of the matrix stored in CacheMatrix object

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}


