## These fuctions are use to create special matrix and cache the inverse of the matrix

## makeCacheMatrix creates a square matrix for caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse.matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse.matrix <<- solve
  getinverse <- function() inverse.matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse.matrix <- x$getinverse()
  if(!is.null(inverse.matrix)) {
    message("getting inverse matrix...")
    return(inverse.matrix)
  }
  data <- x$get()
  inverse.matrix <- solve(data)
  x$setinverse(inverse.matrix)
  inverse.matrix
}
