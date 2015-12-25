## These functions take a matrix and return and cache its inverse. If
## the inverse is already cached, the cached value is returned;
## otherwise the inverse is calculated using the solve function.

## This function creates a list of functions that set and retrieve
## values in the global environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y # sets x equal to y in the global environment
                m <<- NULL # clears cached inverse because y matrix has changed
        }
        get <- function() x # function to retrieve x
        setinverse <- function(inverse) m <<- inverse # function to set inverse
        getinverse <- function() m # function to retrieve cached inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks whether the inverse of a matrix is cached and 
## either retrieves cached matrix or calculates inverse and then caches it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() # retrieves cached value
        if(!is.null(m)) { # checks whether there is a cached value
                message("getting cached data")
                return(m) # returns cached value if there is one
        }
        data <- x$get() # retrieves original matrix
        m <- solve(data, ...) # finds inverse of original matrix
        x$setinverse(m) # caches inverse
        m # returns inverse
}
        

