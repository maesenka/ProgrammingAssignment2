## These functions calculate and cache the inverse of a matrix. When
## the inverse has been calculated once using cacheSolve, any subsequent invocation
## of this function returns the cached value.
## 

## Creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse cache to NULL
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)  
}


## Calculates the inverse of the given matrix, and stores it in the matrix cache
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- 1/data
        x$setinverse(inv)        
        ## Return a matrix that is the inverse of 'x'
        inv
}
