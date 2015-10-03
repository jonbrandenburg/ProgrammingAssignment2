## Pair of functions that cache the inverse of a matrix.  This assumes that
## the matrix supplied is always invertable.

# The first function, makeCacheMatrix creates a special "matrix", which is
# really a list containing a function to
#
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse matrix
#   4. get the value of the inverse matrix

# The second function, cacheSolve calculates the inverse of the special
# "matrix" created with the above function. However, it first checks to 
# see if the inverse has already been calculated. If so, it gets the 
# inverse from the cache and skips the computation. Otherwise, it 
# calculates the inverse of the matrix and sets the value of the inverse
# in the cache via the setinverse function.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) m <<- matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x'
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)

  ## Return a matrix that is the inverse of 'x'
  m
}
