## Assignment 2 for rprog-010 coursera course
## Learning to use the <<- operator and lexical scoping
## <<- assigns a value different to the current environment

## Creates a list of matrices from the input matrix
## List of 4 vectors:   1. set the value of the matrix
##                      2. get the value of the matrix
##                      3. set the value of the inverse
##                      4. get the value of the inverse

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


## Calculates the inverse of the matrix unless a cache of the inverse exists

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
