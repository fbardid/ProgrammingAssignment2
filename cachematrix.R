## Overall description of following functions.
# We have two functions: makeCacheMatrix and cacheSolve.
# The firste one (mackeCacheMatrix) creates a special matrix object 
# that can cache its inverse and the second one (cacheSolve) 
# computes the inverse of the special matrix. However, if the
# inverse has already been calculated, then it gets the inverse from 
# the cache and returns the value. 


## Short comment on the makeCacheMatrix function.
# This function creates a special matrix object which is a list of 4 
# functions to (1) set the value of the matrix, (2) get the value of 
# the matrix, (3) set the value of the inverse, and (4) get the value 
# of the inverse.


makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) ix <<- solve
    getinverse <- function() ix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Short comment describing the cacheSolve function.
# The following function evaluates if the inverse of the special matrix 
# has already been calculated. If this is the case, the inverse from 
# the cache is returned. Else, it gets the data, calculates the 
# inverse of the data, sets the value of the inverse in the cache and 
# returns it.

cacheSolve <- function(x, ...) {
    ix <- x$getinverse()
    if(!is.null(ix)) {
        message("getting cached data")
        return(ix)
    }
    data <- x$get()
    ix <- solve(data, ...)
    x$setinverse(ix)
    ix
}
