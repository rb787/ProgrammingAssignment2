## Two functions below that cache the inverse of a matrix, 
##and solves the inverse if it is not already cached.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        cachedMatrix <- NULL
        set <- function(mtrx) {
                x <<- mtrx
                cachedMatrix <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(solve) cachedMatrix <<- solve
        getInverse <- function() {
                cachedMatrix
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and 
##the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache

cacheSolve <- function(x, ...) {
        cachedMatrix <- x$getInverse()
        if(!is.null(cachedMatrix)) {
                message("getting cached data")
                return(cachedMatrix)
        }
        data <- x$get()
        cachedMatrix <- solve(data, ...)
        x$setInverse(cachedMatrix)
        cachedMatrix
}
