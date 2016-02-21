## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Week 3 Assignment; GitHub user: sumansaurabh13
## makeCacheMatrix returns a list of functions which is used by cacheSolve 
## to compute the inverse of the matrix returned by makeCacheMatrix above
makeCacheMatrix <- function(x = matrix()) { # storing the cached value
  invmat <- NULL # initializing invmat to NULL
  
  # creating the matrix in the working environment
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  # getting the value of the matrix
  get <- function() x
  
  # inverting the matrix and storing in cache
  setinv <- function(inverse) invmat <<- inverse
  
  # getting the inverted matrix from cache
  getinv <- function() invmat 
  
  # returning the created functions to the working environment
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## attempting to get the inverse of the matrix stored in cache
  invmat <- x$getInverse()
  
  # returning inverted matrix from cache if it exists
  # else creating the matrix in working environment
  if (!is.null(invmat )) {
    message("getting cached data")
    
    # displaying matrix in console
    return(invmat)
  }
  
  # creating matrix since it does not exist
  matrix <- x$get()
  
  # making sure matrix is square and invertible
  tryCatch( {
    # set and return inverse of matrix
    invmat <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    # set inverted matrix in cache
    x$setMatrix(invmat)
  } )
  
  # displaying matrix in console
  return (invmat)
}
