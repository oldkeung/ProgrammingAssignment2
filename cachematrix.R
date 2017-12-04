## These functions store the matrix in a special object and calculate 
## the inverse of matrix; or return the cached calculated inverse of matrix 
## if it has already been calculated

## Stores the matrix and the inverse of matrix
## set        set the value of the matrix
## get        get the value of the matrix
## setinverse set the value of the inverse
## getinverse get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculates or returns the cached inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
