


# functions that cache potenitally time consuming computations


# This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function(inverse) {
    i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getIverse)
  
}

# Compute the inverse of special matrix returned by "makeCacheMatrix" above.

cacheSolve <- function(x, ...){
  
  # return a matrix that is inverse of x
  
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %% data
  x$setInverse(m)
  m
}