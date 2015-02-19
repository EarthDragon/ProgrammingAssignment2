## These two functions together calculate and cache the inverse of a matrix.


## This function creates a special "matrix" object (a list
## of functions) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes as input the list of functions returned by
## makeCacheMatrix, and returns the inverse of the original matrix.  
## If the inverse has already been calculated 
## then cacheSolve retrieves the inverse from the cache.
## If the inverse has not already been calculated, then this function
## will compute it and cache the result for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data") 
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}









