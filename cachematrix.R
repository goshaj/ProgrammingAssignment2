## Functions for creating and using inverted matrices which caching ability

## Creates cacheable matrix for inputting to cacheSolve() function which sets and gets cached values

makeCacheMatrix <- function(original.matrix = matrix()) {
  
  # Checks if the input is correct
  if (!is.matrix(original.matrix)) {
    stop("Please give a matrix")
  }
  
  inverted.matrix <- NULL
  
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
  
  # Gets and sets cached inverted matrix value
  get <- function() original.matrix
        
  # Inverses the matrix using build in solve() function in R
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix, cacheSolve() returns the cached inverse

cacheSolve <- function(cacheable.matrix, ...) {
  inverted.matrix <- cacheable.matrix$get.inverse()
        
  # Checks if there is a cached matrix
  if(!is.null(inverted.matrix)) {
    message("Getting cached inverse matrix")
    return(inverted.matrix)
  }
        
  # Creates inversed matrix
  matrix.to.inverse <- cacheable.matrix$get()
  inverted.matrix <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix
  
}
