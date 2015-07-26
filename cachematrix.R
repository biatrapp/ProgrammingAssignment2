## Creates an object that stores a matrix and caches its inverse

## The first function sets and gets the value of the matrix.
## It caches the value of the inverse.

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


## Checks if the inverse of the matrix 'x' was previously calculated. 
## Returns the previously calculated inverse, or calculates the inverse 
## and returns the results.

cacheSolve <- function(x, ...) {
        ## Checks if the inverse of 'x' was already calculated, 
	  ##if yes, return the message "getting cached data" and the inverse.
	  m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	  ##  Gets matrix 'x' and calculates and returns its inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
