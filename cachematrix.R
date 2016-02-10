## Caching the inverse of a matrix to avoid its computation
## everytime

## This function creates a special matrix which
## sets and gets the value of the matrix
## sets and gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## Get the value of the matrix
    get <- function() x
    ## Set the value of the inverse
    setInverse <- function(inverseInput) inverse <<- inverseInput
    ## Get the value of the inverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function calculates the inverse of a matrix x.
## However, it first checks to see if the inverse has
## already been calculated. If so, gets the inverse from
## the cache, and skip the computation. Otherwise, calculate
## the inverse of the matrix, set the value of the inverse
## in the cache via the setInverse function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    ## Check if inverse exists in cache
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    ## Solve for the inverse
    inverse <- solve(data, ...)
    ## Set inverse in the cache
    x$setInverse(inverse)
    inverse
}
