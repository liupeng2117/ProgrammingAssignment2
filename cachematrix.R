## The function Returns the inverse matrix of any non-sigular matrix 
## that you specify. 

## The function makeCacheMatrix create a list, where each element of 
## the list is a function. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # you specify a matrix x and set the initial value of inv to NULL
  # set function reset the value of matrix x to y if necessary 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  # get function get the value of matrix x
  # setinv function stores the inverse matrix to cache inv
  # getinv function get the value of inv
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  # returns a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The function cacheSolve calculates the inverse of the matrix given in
## the above function. First, it will check the cache first, if the cache 
## stores the value of the inverse matrix, the functions returns the inverse
## matrix directly from the cache and skip the following statements. Otherwise,
## This function will slove the matrix and returns its inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  #first check the value of the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if not, caculate the inverse matrix inv, store in cache and returns the value of inv
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
