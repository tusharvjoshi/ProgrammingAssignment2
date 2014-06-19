
## This function will create an supporting environment
## to cache the inverse of a matrix with the original 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  ## make a variable for storing inverse
  my_inverse <- NULL
  
  ## function for setting a new matrix
  ## it will re initialize the inverse stored
  set <- function(y) {
    x <<- y
    my_inverse <<- NULL
  }
  
  ## returns the original matrix stored
  get <- function() x
  
  ## stores the inverse of the matrix
  setinverse <- function(the_inverse) my_inverse <<- the_inverse
  
  ## returns the stored inverse of matrix NULL is not stored yet
  getinverse <- function() my_inverse
  
  ## makes a list of all the functions defined
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function assumes the first argument is of special
## variable created by makeCacheMatrix and uses its functions
## to check whether an inverse is already available to return
## if not available it creates and stores for further use 
## then it returns the inverse

cacheSolve <- function(x, ...) {
  
  ## try to get the inverse from the store
  my_inverse <- x$getinverse()
  
  ## If the inverse is already available 
  ## return it 
  if(!is.null(my_inverse)) {
    message("getting cached data")
    return(my_inverse)
  }
  
  ## get the original matrix
  data <- x$get()
  
  ## compute the inverse
  my_inverse <- solve(data, ...)
  
  ## store the inverse for later use
  x$setinverse(my_inverse)
  
  ## Return a matrix that is the inverse of 'x'
  my_inverse
}
