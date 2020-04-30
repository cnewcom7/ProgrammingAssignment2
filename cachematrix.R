## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## cnewcom7 
## Developed using makeVector/cachemean examples functions from assignment as templates
## makeCacheMatrix takes a matrix argument and cache's its calc'd inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invMat <- NULL    ## set the variable to store the inverse matrix as empty/NULL to start
  
  
  # Resets matrix without needed to call entire makeCacheMatrix
  # Use "variable-name"$set(matrix(a:b, x-dim, y-dim)) format to change matrix and clear cache storage
  set <- function(y= matrix()) {
    x <<- y 
    invMat <<- NULL  
  }
  
  get <- function() x # Returns the original matrix
  
  setInvMat <- function(solve) invMat <<- solve
  
  getInvMat <- function() invMat # Returns what's stored in invMat (either NULL or inverse'd matrix)
  
  # Set identifiers to easily call each function above
  list(set = set, 
       get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)  
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the matrix entered in the above function 

cacheSolve <- function(x, ...) {
  
  # Imports the value stored in invMat
  # Checks if invMat is empty or not
  # If empty then moves to next section to perform inverse calc; if not empty returns the cache'd data
  invMat <- x$getInvMat() 
  
  if(!is.null(invMat)) {
    
    message("Getting cached data...")
    invMat
    
  }
  
  # Import the original matrix and use 'solve' function to calc inverse, then cache output in invMat variable
  matrix <- x$get()  
  invMat <- solve(matrix, ...)  
  x$setInvMat(invMat) 
  
  ## Returns the inverse of the matrix 'x'
  invMat
}