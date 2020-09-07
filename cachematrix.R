## Here are 2 functions that make cached inverse of a given matrix

## This function creates a list with
## functions to set, get the value of a matrix
## and the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse of a matrix
## created in the above function

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("Getting the cashed inverse...")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i ## Return a matrix that is the inverse of 'x'
}
