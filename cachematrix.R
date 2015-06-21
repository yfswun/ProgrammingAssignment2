## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than
## compute it repeatedly

## The functions below cache the inverse of a matrix.

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solveInv) i <<- solveInv
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve:
## This function computes the inverse of the special "matrix"
## returned by the makeCacheMatrix function.
## If the inverse of this matrix has already been calculated,
## then the function retrieves it from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
