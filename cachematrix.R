## This will create a special matrix that will cache it's inverse to save time

## This function sets up the basis matrix functionality

makeCacheMatrix <- function(x = matrix()) {
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


## Returns the inverse of our special matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i))
        {
          message("getting cached data")
          return(i)
        }
        else
        {
          data <- x$get()
          i <- solve(data)
          x$setinverse(i)
           i          
        }
}
