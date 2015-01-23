## Author - RDubash
## Matrix inversion is a costly computation and so there is benefit to caching the
## inverse of a matrix rather than computing it repeatedly. This pair of functions
## compute and cache the inverse of a matrix. The inverse itself is computed using
## the solve() function. It is assumed that the matrix is invertible.
## 

## Function makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL

    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }

    getmatrix <- function () x

    setinverse <- function(solve) m <<- solve

    getinverse <- function () m
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
            setinverse = setinverse, getinverse = getinverse)
    

}


## Function cacheSolve computes the inverse of the special "matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse ()
    if (!is.null(m)) {
        message("getting cached data")
        return (m)
    }

    data <- x$getmatrix ()
    m <- solve(data, ...)
    x$setinverse(m)

    m

}
