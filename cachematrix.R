## Put comments here that give an overall description of what your
## functions do

## This function initializes the cached matrix object container
makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     
     # sets the matrix
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     
     # Returns the matrix
     get <- function() {
          x
     }
     
     # sets teh value of the cahed inverse matrix
     setsolve <- function(mean) {
          inverse <<- mean
     }
     
     # returns the cached inverse matrix
     getsolve <- function() {
          inverse
     }
     
     # Finally return the object.
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)     

}


## This function process the inverse of the given cached matrix object 
## when the value og the inverse is not already set.
cacheSolve <- function(x, ...) {
     inverse <- x$getsolve()
     
     # if the inverse already was procesed return the cached value
     if(!is.null(inverse)) {
          return(inverse)
     }
     
     data <- x$get()
     
     # Calculate the inverse of teh data matrix
     inverse <- solve(data, ...)
     
     # Store the value of the calculated inverse into the object
     x$setsolve(inverse)
     
     # Return the inverse matrix value
     inverse
}