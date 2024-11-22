## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL  # Reset the cached inverse when the matrix changes
  }
  
  ## Method to get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Check if the inverse is already cached
  m <- x$getInverse()
  
  ## Return the cached inverse if it exists
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse of the matrix
  m <- solve(data, ...)
  
  ## Set the inverse in the cache
  x$setInverse(m)
  
  ## Return the computed inverse
  m
}
