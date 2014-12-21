##R Programming Assignment 2

## These two functions work together to define a matrix to be evaluated.
## The inverse of this matrix is sought. If the inverse has not previously been calculated, it
## is calculated and assigned to the matrix. If the inverse has already been calculated, it is
## called from memory.

## A new matrix can be set, this erases any matrix inverse that existed in memory and begins the
## process over.

## This function sets the matrix to be evaluated as well as the associated inverse.
## Calling the original matrix and its inverse are also established.

makeCacheMatrix <- function(x = matrix()) {             ##assign matrix as function argument
        m <- NULL                                       ##matrix inverse is set to NULL
        set <- function(y) {
                x <<- y                                 ##change the evaluated matrix
                m <<- NULL                              ##matrix inverse is reset to NULL
        }
        get <- function() x                             ##print matrix x
        setinverse <- function(solve) m <<- solve       ##inverse m is assigned outer environment
        getinverse <- function() m                      ##print inverse m associated with matrix x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## This function returns a matrix that is the inverse of 'x'.
## It calls the cached inverse if it exists or calculates the inverse if it doesn't exist 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                             ##call 'm', the inverse of matrix x
        if(!is.null(m)) {
                message("getting cached data")          ##if m is NOT blank, print m
                return(m)                               ##end function here
        }
        data <- x$get()                                 ##data is x matrix
        m <- solve(data, ...)                           ##m is the inverse of data (matrix x)
        x$setinverse(m)                                 ##set the inverse of x equal to m in other first
                                                        ##funtion environment
        m                                               ##print m
}
