## Creates and stores a matrix to a variable in a 
## new environment then solves for the inverse of 
## the matrix. If the matrix already exists in new 
## environment, uses the cached variable instead
## of solving for the inverse again.

# Takes a nonsingular invertible matrix as an
# argument and stores (caches) it in memory

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Returns the inverse of a nonsingular invertible
# matrix. If matrix is previously solved for and
# cached, return cached value instead of solving again.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
