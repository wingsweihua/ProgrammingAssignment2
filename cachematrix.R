
## This function creates a special "matrix",a list containing a function to
## get & set the value of the matrix, get & set the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}



## This function return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ##it first checks to see if the inverse has already been calculated.
  i <- x$getinverse()
  ## If so, it gets the inverse from the cache and skips the computation.
  if(!is.null(i)) { 
    message("getting cached data")
    return(i)
  }
  ##if not,calculate it 
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

