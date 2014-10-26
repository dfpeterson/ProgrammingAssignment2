
## This function creates a matrix that can return the inverse
## of a squarable matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- solve
  getinverse <- function() solve(x)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}
## cacheSolve returns a cached inverse of the matrix
## if one is available
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
  
  ## Return a matrix that is the inverse of 'x'
}