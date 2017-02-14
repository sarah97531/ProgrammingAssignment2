## These functions output the inverse of a set matrix, checking first 
## to see if the inverse has already been cached. If so the cached inverse is 
## printed; if not, the inverse is calculated and printed.

## makeCacheMatrix function creates a list containing functions which get and 
## set the values of the matrix and the inverse of the matrix and caches the 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the cached inverse of the matrix if available.
## If no cached inverse is found (getinverse = NULL) the inverse is computed.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
