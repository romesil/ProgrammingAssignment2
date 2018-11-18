# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it 
# repeatedly. 
# Below are a pair of functions that cache the inverse of a matrix.

### This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    
    inv <- NULL                ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {      ## define the set function to assign new 
      x <<- y                 ## value of matrix in parent environment
      inv <<- NULL            ## if there is a new matrix, reset inv to NULL
    }
    get <- function() {x}      ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(solve) {inv} <<- solve  ## assigns value of inv in parent environment
    getinverse <- function() {inv}                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
    
    
  }
  
 
### This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix function. If the inverse has already been 
# calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    
 #   x <- as.list(x) # need to ensure x is a list and non atomic, so $ below can be applied
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
