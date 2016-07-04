## R Programming Assignment 2
## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
## Sets and gets value of matrix, then sets and gets value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}


## This function computes inverse of the special "matrix" from makeCacheMatrix.
## If inverse has previously been calculated, that result will be retrieved.
## Otherwise, inverse will be computed for the first time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
