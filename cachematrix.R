## Week 3 Assignment
## This script caches the inverse of the supplied matrix

## This creates a list that contains setters and getters for the supplied 
## matrix x
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        ## Updating x means the cached data needs to be cleared
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inver) m <<- inver
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## Returns a matrix that is the inverse of 'x'
## If the inverse was already solved for x then this is returned otherwise it 
## determines the inverse of x, updates the inverse of x and returns the inverse 
## matrix
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ## m is NULL so calculating the inverse matrix
    data <- x$get()
    ## solve returns the inverse of the supplied matrix
    m <- solve(data)
    ## Caching the inverted matrix
    x$setInverse(m)
    m
}
