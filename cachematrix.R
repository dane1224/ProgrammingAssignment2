## These functions allow for quicker calculation of a matrix inverse.
## By caching the inverse of a created matrix with makeCacheMatrix,
## the inverse of the matrix is then found with cacheSolve. If the 
## inverse of the matrix has already been found, cacheSolve simply
## looks up the already cached value rather than solving again.

## This function creates an object that caches the matrix and its inverse.

makeCacheMatrix <- function(A = matrix()) {
    I <- NULL 
    setA <- function(y) { ## When setA is called, rewriting A with input y.
        A <<- y
        I <<- NULL
    }
    ## Writing functions for getting matrix A, setting Inverse, and getting Inverse.
    getA <- function() as.matrix(A) 
    setI <- function(A_inv) I <<- A_inv 
    getI <- function() I 
    list(setA = setA, getA = getA, setI = setI, getI = getI) 
}


## This function computes the inverse of the special object from makeCacheMatrix.
## It retrieves the value of the inverse if already cached, or calculates the inverse from scratch, if not.
## The function ultimately returns a matrix that is the inverse of 'x'.

cacheSolve <- function(A, ...) { ## A is the result of a makeCacheMatrix.
    I <- A$getI() 
    if(!identical(I,NULL)) { ## Checking if cached inverse is not NULL.
        message("obtaining cached data for inverse") 
        return(I) 
    }
    ## Setting the cached value of A, solving for I, and setting the cached value of I.
    A$setA(A$getA()) 
    I <- solve(A$getA(), ...) 
    A$setI(I) 
    I 
}