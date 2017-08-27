## This function allows for the creation of matrices that can cache
## the inverse of such matrices.


## This function allows for the creation of a matrix in a way
## that allows for the caching of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function returns the inverse of a matrix and caches it.
## It must take an object created by the makeCacheMatrix as its
## input. If the object passed to it does not have its inverse
## cached it will use the solve() function to calculate it and 
## will store it in the cache as well as return it. If the 
## inverse is already available in the cache it will return that.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
