## These functions calculate the Inverse of a Matrix
## or retrive the cached value.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMat <- function(InvMat) m <<- InvMat
  getInvMat <- function() m
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
  
  
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getInvMat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMat(m)
  m
  
}
