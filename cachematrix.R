## The functions 'makeCacheMatrix' and 'cacheSolve' combine to create and cache a
## matrix and its computed inverse. As the information is cached, it is quickly and 
## easily accessable for future applications without requiring redundant computational
## power.

## 'makeCacheMatrix' creates a list composed of the functions to:
## 1.  set the value of a described matrix
## 2.  get the value of a described matrix
## 3.  set the value of the inverse of the described matrix
## 4.  get the value of the inverse of the described matrix

makeCacheMatrix <- function(x = matrix()) {
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

## 'cacheSolve' solves the inversion for the matrix described in 'makeCacheMatrix'.
## If the information is cached, it returns the inverted matrix; otherwise, 
## cacheSolve solves the matrix inversion and returns the inverted matrix.

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
