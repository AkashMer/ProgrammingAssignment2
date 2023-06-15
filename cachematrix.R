## The following functions cache the inverse of a matrix and
## return the cached value if not null, otherwise calculates
## the inverse of a matrix and caches it for later use

## Creates a "special" Matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Calculates the inverse of matrix x, 
## or gets the inverse if already cached

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Gettin the cached matrix inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
