## This function is for a pair of functions that cache the inverse of a matrix
## Assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize inverse value
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv<- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Check if the inverse has been calculated, if so, then use the value from cache
# If the inverse doesn't exist in catche, calcualte it

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  # Inverse exist
  if(!is.null(inv)) {
    
    message("getting cached data")
    
    ## Return a matrix that is the inverse of 'x'
    return(inv)
  }
  
  # Inverse does not exist, calculate inverse and set the inverse value back
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  return(inv)   
}

# Test result
#k <- matrix(c(4,1,3,1),2,2) 
#k
#tmp = makeCacheMatrix(k)
#cacheSolve(tmp)
