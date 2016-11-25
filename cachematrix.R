## A pair of functions that cache the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to:

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inversa <- NULL
    set <- function(y) {
      x <<- y
      inversa <<- NULL
    }
    get <- function() x
    setinversa <- function(inverse) inversa <<- inverse
    getinversa <- function() inversa
    list(set = set, get = get, setinversa = setinversa, getinversa = getinversa)
}


## The following function calculates the inverse of the special
## "matrix" created with the above function. However, it first
## checks to see if the inverse of the matrix has already been 
## calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix
## of the data and sets the value of the inverse in the cache via
## the setmean function.

cacheSolve <- function(x, ...) {
       inversa <- x$getinversa()
       if(!is.null(inversa)) {
         message("getting cached data")
         return(inversa)
       }
       data <- x$get()
       inversa <- solve(data,...)
       x$setinversa(inversa)
       inversa
}
