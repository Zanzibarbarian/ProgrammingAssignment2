## The two functions below interact primarily based on whether or not m == NULL.
## If m == NULL, then makeCacheMatrix has already solved the matrix and cached
## the inverse. If m does not equal NULL, then cacheSolve solves the inputted
## matrix regardless of cached info.




## This function receives a matrix as input, and caches its inverse matrix.
## When the inverse matrix is cached, m is set to NULL (which cacheSolve
## looks for later).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}




## This function uses an if loop to check to to see if m == NULL. If m == NULL, 
## the function retrives the cached inverse matrix from makeCacheMatrix. 
## If m != null, then the function solves the inputted matrix and spits out
## the inverse (m).

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("retrieving cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}

