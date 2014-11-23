## These functions allows for matrix inverse computation with caching using scoping. 


## Creates an object holding a matrix which caches its inverse (solve function)
##
## Sample usage:
##     > m <- matrix(c(2,3,2,2),2,2, byrow=TRUE)
##     > cached_m <- makeCacheMatrix(m)
##     > cached_m$get()
##          [,1] [,2]
##     [1,]    2    3
##     [2,]    2    2

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Computes matrix inverse on the matrix object (created with above function) using 
## the solve function. If the inversion was computed previously, cached result is used.
## 
## Sample usage:
##     > cacheSolve(cached_m)
##     [,1] [,2]
##     [1,]   -1  1.5
##     [2,]    1 -1.0
##     > cacheSolve(cached_m)
##     getting cached data
##     [,1] [,2]
##     [1,]   -1  1.5
##     [2,]    1 -1.0

cacheSolve <- function(x, ...) {
    ## try to get the value of the previous solve call and return it
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## otherwise get the data, call solve, cache the result and return it
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
