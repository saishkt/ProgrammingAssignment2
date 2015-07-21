## cachematrix.R will help speed up repeat calculation of inverse of a matrix through caching.
## 
## there are 2 functions that shall help you achieve this ... Their description follows the example below
## Example 1:
## 
## mat <- matrix(1:4, nrow=2, ncol=2)
##
## cacheMat <- makeCacheMatrix(mat) # Creates a cache enabled matrix store
##
## cacheSolve <- cacheSolve(cacheMat)   # since inverse is caculated for first time... 
##                                      # inverse will be calculated and stored in cache
##
## cacheSolve <- cacheSolve(cacheMat)   # sinve inverse is already cached... 
##                                      # inverse will be directly retreived from the cache

## macheCacheMatrix:
## This function creates and returns a cached enabled matrix that can be later passed on to the 
## cacheSolve function to calculate a cache enabled inverse of  the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve:
## This function calculates and returns the inverse of a matrix x 
## By default cacheSolve will look up the cache to see if the inverse exists 
## else will calculate and return the inverse of matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get();
        m <- solve(data)
        x$setinverse(m)
        
        m
}
