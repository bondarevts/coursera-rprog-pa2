## This functions allow to cache matrix solve from the first call to another calls

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(new_matrix) {
    x <<- new_matrix
    cached_inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(new_inverse) cached_inverse <<- new_inverse
  getsolve <- function() cached_inverse
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This function computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix above. If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should retrieve 
##   the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversed <- x$getsolve()
  if (!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  matrix <- x$get()
  inversed <- solve(matrix, ...)
  x$setsolve(inversed)
  inversed
}
