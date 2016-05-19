# Sample to test the small program
# source("cachematrix.R") load the source saved in your working directory
# x <- matrix(rnorm(16), 4,4)   - creates a sample matrix
# cache_x <- makeCacheMatrix(x) - creates the special matrix object
# cacheSolve(cache_x)           - returns the inversed matrix  
# cacheSolve(cache_x)           - the 2nd run returns the cached inversed matrix


#  makeCacheMatrix: This function creates a special "matrix" object that caches its inverse.
#  set the value of the matrix
#  get the value of the matrix
#  setm the value of the inverse
#  getm the value of the inverse




makeCacheMatrix <- function(x = matrix()) {
  # m is used to store the inverse matrix
  m <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverses matrix
  setm <- function(inverse) m <<- inverse
  
  # Get the inversed matrix
  getm <- function() m
  
  # Return the matrix with the defined functions
  list(set = set, get = get, setm = setm, getm = getm)
}


# cacheSolve - compute the inverse of the special matrix returned by makeCacheMatrix 
# if the inversed matrix exists already in the cache it will be returned, otherwise it will be calculated.


cacheSolve <- function(x, ...) {
  m <- x$getm()
  
  # If the inversed matrix exists in the cache just return it
  
  if (!is.null(m)) {
    message("getting the cached inversed matrix")
    return(m)
  }
  
  # if the inversed matrix does not exist in cache we calculate it
  
  data <- x$get()
  m <- solve(data, ...)
  
  # Cache the result
  x$setm(m)
  
  # Return the result
  m
}
