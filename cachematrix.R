################################################################################
## This source contains the following two R fuctions to demonstrate caching:   #
##                                                                             #
## makeCacheMatrix: creates a special "matrix" object that can cache its       #
##                  inverse                                                    #
##                                                                             #
##-----------------------------------------------------------------------------#
##                                                                             #
## cacheSolve     : This function computes the inverse of the special "matrix" #
##                  returned by makeCacheMatrix above. If  inverse has already #
##                  been calculated (and the matrix has not changed), then     #
##                  cacheSolve should retrieve the inverse from the cache      #
##                                                                             #
################################################################################


##------------------------------------------------------------------------------
## To creates a special "matrix" object that can cache
## It takes a matrix as a parameter and 
##------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  # Setter and getter functions that will define  behaviour of the special matrix.
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x

  setsolve <- function(solve) s <<- solve

  getsolve <- function() s

  # The special matrix that contains necessary functions to create return inverse  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##------------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix.
##------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {

  ## Try to get the inverse, if possible from Cache  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## Not found in cache, hence calculating the inverse and caching it for 
  ## Subsequent usage.
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)

  ## Return a matrix that is the inverse of 'x'
  s
}
