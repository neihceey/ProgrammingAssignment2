## The aim of this assignment is to write a pair of functions that cache the inverse of a matrix 
## so that the inverse do not have to be done repeatedly if the matrix has not changed.

## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function computes the inverse of the matrix returned by the makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve (data, ...)
        x$setInverse(m)
        m   
}
