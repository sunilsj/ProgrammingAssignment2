## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function is a special Matrix that has a capability
# to cache the inverse of the matrix supplied via the parameter
makeCacheMatrix <- function(x = matrix()) {
    # Inverse value, set to NULL to begin with
    inverse <- NULL
    
    # Setter function to put the new matrix data
    setMatrix <- function(newMatrix) {
        # Invalidate the inverse matrix that will be treated as a cache
        inverse <<- NULL
        # Set the new matrix to the parameter level variable
        x <<- newMatrix
    }
    
    # Gets the original Matrix that was set as a parameter
    getMatrix <- function() {
        x
    }
    
    # Set inverse value
    setInverse <- function(i) {
        inverse <<- i
    }
    
    # Get the inverse value
    getInverse <- function() {
        inverse
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
    
}


## Write a short comment describing this function
# This function tests if the inverse of a matrix is already
# available with the special matrix. If it is not, it will
# compute the inverse of the matrix, store that value in 
# the special matrix that will get cached and return the value
cacheSolve <- function(x, ...) {
    # Acquire the inverse value from the Cache Matrix object
    inverse <- x$getInverse()
    
    # Test to see if the object is Not NULL, that means Inverse has been 
    # available in cache
    if (!is.null(inverse)) {
        message ("getting cached inverse data")
        # By doing return below, the cached data is returned. 
        # Any further statements will not be executed
        return (inverse)
    }
    # If the flow comes here, it means the cache value was found to be NULL
    # Get the original matrix data
    data <- x$getMatrix()
    
    # Inverse the Matrix
    inverse <- solve(data, ...)
    
    # Set the inverse data into special matrix object.
    x$setInverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
