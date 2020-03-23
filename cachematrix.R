## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function does the caching. The special "CacheMatrix"
# can use this function to cache the result. Note that none computation
# is not done in this function. It just keeps record of the result.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {m <<- inverse}
  getInverse <- function() {m}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# If the x had been solved before, then it would be stored in cache.
# In this case, we directly retrieve the result from the cache instead
# of solving it again
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Computed and cached before.")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
