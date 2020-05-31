## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function stores the matrix in a cache. The input for this function is a matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## The function below returns the inverse of a matrix. It's input is a matrix or a matrix stored in a cache based on the makeCacheMatrix function. It will return the input matrix if the cache already has the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
