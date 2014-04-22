## The functions below create a special "matrix" object that can cache its inverse. The first function, 
## makeCacheMatrix, creates a special "matrix" object/list that can be used to cache the inverse of the 
## matrix. In addition, it defines several helper functions that can be used to query/access the input 
## matrix and/or the inverse of the input matrix. The second function, cacheSolve, checks within the 
## special "matrix" object to see whether the inverse of the input matrix stored exists. If the inverse 
## of the input matrix exists, the function returns the value stored in the special "matrix" object; if 
## the inverse of the input matrix does not exist, the function calculates the inverse of the matrix 
## and stores/caches its value in the special "matrix" object, and returns the value of the inverse of 
## the input matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## This function takes a matrix and creates four functions, a) set, b) get, c) setinv, and d) getinv 
  ## which can be used to a) store the input matrix or modify/replace the matrix, b) retrieve the input 
  ## matrix, c) calculate and store the inverse of the input matrix, and d) retrieve the inverse of the 
  ## input matrix.
  
  m <- NULL
  set <- function(y) { 
    # This function stores the input matrix as "x" or allows one to reset/update the input matrix as 
    # specified
    x <<- y # Stores the user specified input matrix
    m <<- NULL # Reset the calculated inverse to NULL because the inverse of the specified input matrix
               # has has not yet been calculated
  }
  get <- function() x # This function retrieves "x"
  setinv <- function(ginv) m <<- ginv # This function stores the inverse of "x" as "m" or allows one to 
                                      # reset/update the inverse of "x" as specified
  getinv <- function() m # This function retrieves "m"
  list(set = set, get = get, setinv = setinv, getinv = getinv) # Creates a list of the functions created 
                                                               # by the function "makeCacheMatrix"
}

cacheSolve <- function(x, ...) {
  ## This function's input is a makeCacheMatrix list. The function will calculate the Moore-Penrose 
  ## generalized inverse of the input matrix stored in the makeCacheMatrix list. If the inverse of the 
  ## matrix already exists within the makeCacheMatrix list, this function will simply return that value.
  
  m <- x$getinv() # Checks the makeCacheMatrix list to see if the inverse of the input matrix has 
                  # already been calculated
  if(!is.null(m)) { 
    # This function checks if the inverse has already been calculated (i.e., is not NULL), it returns 
    # the stored value and exits the function
    message("getting cached inverse") # Writes the message "getting cached inverse" to the console
    return(m) # returns the value of the inverse and exits the function
  }
  data <- x$get() # This function extracts the input matrix from the makeCacheMatrix list
  require(MASS) # Loads the package "MASS" where the function "ginv" exists
  m <- ginv(data, ...)  # Calculates the Moore-Penrose generalized inverse of a matrix
  x$setinv(m) # Stores the inverse of the input matrix within the makeCacheMatrix list
  m # Returns the value of the inverse of the input matrix
}
