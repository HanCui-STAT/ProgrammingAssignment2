##  a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    ## set the value of the matrix
    setMatrix <- function(y){
        x <<- y
        inverseMatrix <<- NULL
    }
    ## get the value of the matrix
    getMatrix <- function() x
    ## set the value of the inverse of the matrix
    setInverseMatrix <- function(invMatrix) inverseMatrix <<- invMatrix
    ## get the value of the inverse of the matrix
    getInverseMatrix <- function() inverseMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverseMatrix()
    ## check if it is not null, then return the inverse
    if (!is.null(inverseMatrix)){
        message("getting cached data")
        return (inverseMatrix)
    }
    ## if it is null, then calculate the inverse
    data <- x$getMatrix()
    inv <- solve(data, ...)
    x$setInverseMatrix(inv)
    inv
}
