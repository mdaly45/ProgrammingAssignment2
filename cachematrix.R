## makeCacheMatrix creates a special matrix which is a 
## list containing the functions set, which sets the 
## matrix, get, which gets the matrix, setinv, which sets
## the inverse of the matrix, and getinv, which gets the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(s) {
          x <<- s
          i <<- NULL
     }
     gc <- function() x
     setinv <- function(inverse) i <<- inverse
     getinv <- function() i
     list(set = set, gc = gc, setinv = setinv, getinv = getinv)
}

## cacheSolve calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
     i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$gc()
     i <- solve(data, ...)
     x$setinv(i)
     i
}
