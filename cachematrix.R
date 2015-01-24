## Author - RDubash
## Matrix inversion is a costly computation and so there is benefit to caching the
## inverse of a matrix rather than computing it repeatedly. This pair of functions
## compute and cache the inverse of a matrix. The inverse itself is computed using
## the solve() function. It is assumed that the matrix is invertible.
## 
## Here are a couple examples of the usage and output of these functions in R:
## > x = rbind(c(1,0),c(0,1))
## > x
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > m <- makeCacheMatrix(x)
## > m$getmatrix()
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > cacheSolve(m)
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > x = rbind(c(1,-1/4),c(-1/4,1))
## > x
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > m <- makeCacheMatrix(x)
## > m$getmatrix()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(m)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 


## Function makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse. The four functions are:
## setmatrix - sets the matrix to the one that you pass as an argument
## getmatrix - returns the matrix that has been stored/cached
## setinverse - sets the inverse of the matrix to the value passed in as argument
## getinverse - returns the inverse of the matrix that has been stored/cached

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL

    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }

    getmatrix <- function () x

    setinverse <- function(inv_mat) inv <<- inv_mat

    getinverse <- function () inv
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
            setinverse = setinverse, getinverse = getinverse)
    

}


## Function cacheSolve computes the inverse of the special "matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inv <- x$getinverse ()
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }

    data <- x$getmatrix ()
    inv <- solve(data)
    x$setinverse(inv)

    inv

}
