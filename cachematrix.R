## This function takes a matrix as input and outputs the inverse of it
## An inverse function can be chached and displayed later when this funciton is run
## If inverse is not set this function will display the actual inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix(numeric(), nrow=2, ncol=2)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function will inverse the matrix and display
## if no matrix is chached before
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
