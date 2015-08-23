## This function creates a special 'matrix' object with 
## four functions for setting and getting matrix data and inverse. 
makeCacheMatrix <- function(x = matrix()) {
        matrixSolve <- NULL
        set <- function(y) {
                x <<- y
                matrixSolve <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) matrixSolve <<- solve
        getSolve <- function() matrixSolve
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function takes the makeCacheMatrix result as an input and returns 
## the matrix inverse. If the inverse was not calculated before, it is 
## calculated and cached in this function.
cacheSolve <- function(x, ...) {
        matrixSolve <- x$getSolve()
        if(!is.null(matrixSolve)) {
                message("getting cached data")
                return(matrixSolve)
        }
        data <- x$get()
        matrixSolve <- solve(data, ...)
        x$setSolve(matrixSolve)
        matrixSolve  
}
