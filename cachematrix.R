


## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set a new matrix and reset the cached inverse
  set <- function(y) {
    x <<- y       # Assign the matrix to the object
    inv <<- NULL  # Reset cached inverse
  }
  
  # Function to retrieve the matrix
  get <- function() {
    x
  }
  
  # Function to cache the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Function to get the cached inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of functions to access and manipulate the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  # Try to get the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, get the matrix from the special object
  data <- x$get()
  
  # Compute the inverse of the matrix
  inv <- solve(data, ...)
  
  # Cache the inverse for future use
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
