## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverted <- function(solve) m <<- solve
        getInverted <- function() m
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)

}


## Calculates the inverse of the "matrix" object entered as parameter,
## if it is already calculated, it retrieves the result from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverted()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverted(m)
        m
}
