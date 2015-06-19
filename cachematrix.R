## makeCacheMatrix: This function centralizes in its environment the allocation of
## the original matrix and the inverse matrix objects. At the same time returns a
## list with all the functionalities necessary for implement the cache pattern. The
## key concept here is that all the functionalities from the returned list work with
## the objects allocated in the makeCacheMatrix environment.
## cacheSolve: This function uses the functions returned in the list from makeCacheMatrix. 
## Firstly checks if the inverse is cached in the makeCacheMatrix environment, if it is 
## then the inverse is returned. In case the inverse is not cached then the original 
## matrix is recovered from makeCacheMatrix environment in order to calculate the inverse, 
## after the calculation is done the object with the inverse is finally cached and returned.

## This function returns a list containing the functions used for caching the inverse 
## of the matrix.

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


## This function solves the inverse of a matrix, but first checks to see if the inverse
## has already been calculated. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
