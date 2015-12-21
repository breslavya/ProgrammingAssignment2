## Creates a matrix-like object that can cache its inverse to avoid
## re-computing it

## makeCacheMatrix creates the matrix object with functions to get/set
## the matrix and to calculate/set the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Default inverse is NULL (note - inverse is reset when matrix changes)
  mat_inv <- NULL
  
  ## Defines the object functions
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setinv <- function(input_inv) mat_inv <<- input_inv
  getinv <- function() mat_inv
  
  ## Returns a list with the relevant functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve determines whether the inverse has already been calculated, and
## if not, calculates the inverse and saves it in the matrix object

cacheSolve <- function(x, ...) {
  
  ## First checks whether the inverse has been calculated, and if so, returns it
  mat_inv <- x$getinv()
  if(!is.null(mat_inv)) {
    message("returning cached inverse")
    return(mat_inv)
  }
  
  ## Otherwise calculates the inverse, caches it and returns it
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setinv(mat_inv)
  mat_inv
}
