## This functions allow to cache matrix solve from the first call to another calls

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  solve <- NULL
  set <- function(new) {
    x <<- new
    solve <<- NULL
  }
  get <- function() x
  setsolve <- function(m_solve) solve <<- m_solve
  getsolve <- function() solve
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This function computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix above. If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should retrieve 
##   the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  slv <- x$getsolve()
  if (!is.null(slv)) {
    message("getting cached data")
    return(slv)
  }
  data <- x$get
  slv <- solve(data)
  x$setsolve(slv)
  slv
}
