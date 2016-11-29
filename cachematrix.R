## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix:
## Takes an invertible square matrix and creates a list from it.
## The list is made of 4 functions:
## get (get matrix)
## set (set matrix)
## getinverse (gets the inverse of matrix)
## setinverse (sets the inverse of matrix)
## Note that the actual inverse computation is not performed here but as needed in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

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
