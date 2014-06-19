## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  my_inverse <- NULL
  set <- function(y) {
    x <<- y
    my_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(the_inverse) my_inverse <<- the_inverse
  getinverse <- function() my_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  my_inverse <- x$getinverse()
  if(!is.null(my_inverse)) {
    message("getting cached data")
    return(my_inverse)
  }
  data <- x$get()
  my_inverse <- solve(data, ...)
  x$setinverse(my_inverse)
  my_inverse
}
