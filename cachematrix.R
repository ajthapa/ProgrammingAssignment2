# Caching of Inverse is a pair of functions makeCacheMatrix(x=matrix()) and cacheSolve(x,...)
# Caching for matrix inversion is done because it is a costly computation. 

## makeCacheMatrix (x) Function
### set(y)          : sets value of the matrix
### get()           : gets value of the matrix
### setinverse(inv) : set the value of the inverse matrix
### getinverse()    : gets the value of  the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve(x,...)
### Calculates the inverse of the special "cache" matrix using solve(data,...) matrix
### The function first checks if the inverse of matrix x is already calculated and returns the inverse if already existed in cache
### Otherwise the function gets the matrix (x$get()) and calculate inverse of the matrix (using solve()) and caches the calculated inverse (setinverse(i)) and finally returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
