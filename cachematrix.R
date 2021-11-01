
#This function creates and returns a matrix-like list from a matrix and initializes the inverse as NULL.

makeCacheMatrix <- function(x = matrix()) {
  
  
  i <- NULL
  
  # A function to set the matrix, and reset the inverse to NULL.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Functions to return the matrix, set its inverse, and return its inverse. 
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Returns the inverse of a makeCacheMatrix.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x', from the cache if it has been calculated before.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #Returns the calculated inverse if the inverse had previously been NULL, and stores the inverse.
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
