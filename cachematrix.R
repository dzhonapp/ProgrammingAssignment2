
makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
    setmatrix <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    getmatrix <- function() x
    setmatrixinv <- function(givenmatinv) matinv <<- givenmatinv
    getmatrixinv <- function() matinv
    
    ## return the list of functions for the matrix.
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setmatrixinv = setmatrixinv,
         getmatrixinv = getmatrixinv)
}


## The function cacheSolve computes the inverse of the special matrix 
## returned by the above function. If the inverse has already been calculated
## it returs the cached value. This function assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matinv <- x$getmatrixinv()
    if(!is.null(matinv)) {
        message("getting cached matrix inverse")
        return(matinv)
    }
    
    ## Cached value not present so calculating the inverse and caching
    data <- x$getmatrix()
    matinv <- solve(data, ...)
    x$setmatrixinv(matinv)
    matinv
}