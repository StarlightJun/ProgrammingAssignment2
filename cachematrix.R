## This R code is for caching the Inverse of a Matrix. A pair of functions
## (makeCacheMatrix and cacheSolve) cache the inverse of a matrix.

## To understand the capabilities of these pair functions, please refer to the 
## test result shown below. This R code assume that the matrix supplied is 
## always invertible.

## Sample test result in R studio
## > y <- makeCacheMatrix()
## > y$get()
##      [,1]
## [1,]   NA
## > cacheSolve(y)
##      [,1]
## [1,]   NA
## > z <- matrix(1:4,2,2)
## > y$set(z)
## > y$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(y)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > zz <- z
## > y$set(zz)
## > cacheSolve(y)
## getting cached matrix inversion
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## The first function makeCacheMatrix creates a special "matrix" object that can
## cache its inverse. This special matrix object is really a list containing a 
## function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                if (!identical(x,y)) {##if the matrix has been changed
                        x <<- y
                        i <<- NULL
                }                     ##if the matrix has not changed,do nothing     
        }        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse        
        getinverse <- function() i 
        
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse = getinverse)
}

## The second cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed 
## - the identical function in makeCacheMatrix above checks whether if there 
## are any changes ), then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {         ## If the inverse has already been calculated
                message("getting cached matrix inversion")
                return(i)
        }
 
        matrixdata <- x$get()
        i <- solve(matrixdata, ...)
        x$setinverse(i)
        i
}
