## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Return a list of 4 functions: storing a matrix, retrieving a matrix, storing
  # the inverse, and retrieving a stored inverse
  inv <- NULL #It's to store the cache inverse matrix
  set <- function(y) {
    # Store input matrix into cache
    x <<- y
    inv <<- NULL #reset cache inverse matrix
  }
  get <- function() x #Retrieve the input matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Calculate the inverse of the matrix, get the result if in cache
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # If the inverse exists, get the inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...) # Calculate inverse matrix
  x$setInverse(inv) #Cache the inverse into setInverse
  inv
        ## Return a matrix that is the inverse of 'x'
}
