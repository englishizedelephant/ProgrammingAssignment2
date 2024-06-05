## This function calculates the inverse of a matrix. 
## If the inverse has already been calculated for the given matrix,
## the stored inverse will be returned instead of recalculating it. 
## Otherwise, the inverse will be calculated and stored for future use.

## This function is a list of function that stores a matrix and 
## its Inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function returns the inverse of a matrix. 
## If the inverse has already been calculated, 
## it will return the stored value. Otherwise, 
## it will calculate the inverse and return the result.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
