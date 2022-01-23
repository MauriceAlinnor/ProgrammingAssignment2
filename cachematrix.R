

## This function returns the inverse of an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## returns a list containing functions to
  ##      1. set the matrix
  ##      2. get the matrix
  ##      3. set the inverse
  ##      4. get the inverse
  ##      this list is used as the input to cacheSove()
  
  inv = NULL
  set = function(y) {
    # assign a value to object in different environment
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of matrix input in makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## x is output of makeCacheMatrix()
  ## returns inverse of the original matrix input
  inv = x$getinv()
  
  ## if the inverse has already been calculated 
  if (!is.null(inv)){
    ## get it from the cache and skip computation message
    return(inv)
  }
  
  ## else, it would calculate the inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  ## set the value of the inverse in the cache through the 
  ## setinv function
  x$setinv(inv)
  
  return(inv)
}
