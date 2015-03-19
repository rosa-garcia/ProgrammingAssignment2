
## This function creates a special vector, wrapping the input matrix and caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## 'x' is the matrix that will be associated with some functions
  ## to allow caching of its inverse. By default is an empty matrix.
  
  ## Return a vector with functions to access (set and get) the input 
  ## matrix and its cached inverse
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix accesible in the input vector
## in an efficient way using cache capabilities

cacheSolve <- function(x, ...) {
 
  ## 'x' is a vector with access to a matrix and its cached inverse
  
  ## Return a matrix that is the inverse of 'x'
    
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
