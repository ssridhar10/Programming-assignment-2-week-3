##The assignment requires a pair of functions that cache the inverse of a matrix.
##The function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##initialize the matrix to NULL
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse <<- inverse
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function below computes the inverse of the special "matrix" that was created by makeCacheMatrix above. If the matrix has not changes and the inverse has already been calculated, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting data that is cached")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}
