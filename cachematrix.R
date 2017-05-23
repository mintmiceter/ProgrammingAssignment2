## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix is a function which creates a matrix that can cache
# its inverse. cacheSolve returns inverse of matrix if in cache, if not
# cacheSolve calculates and returns the inverse

## Write a short comment describing this function
# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# if there is a cached inverse matrix, the function cacheSolve will return that.
# if no cached inverse matrix exists, the function will compute and return the
# inverse of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
