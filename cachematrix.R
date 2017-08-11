## Usage: Suppose you have a large invertible matrix
## For example: 
## > X <- matrix(rnorm(4000000), nrow=2000, ncol=2000)
##
## With the "random" values from rnorm, this is most likely to be invertible. Alternatively,
## find some random seed that works and stick with it
##
## Create the "CacheMatrix" copy of this matrix
## > CX <- makeCacheMatrix(X)
##
## Then use cacheSolve on this "CacheMatrix" CX to get the inverse
##
## > cacheSolve(CX)
##
## The first time this is run, it will have to compute the inverse of X
## Subsequent times, this will just fetch the cached copy of the inverse and is faster.
##
## Notes:
## 1. If you run > CX <- makeCacheMatrix(X) again, it will reset and clear the cache 
## as it re-initializes your "CacheMatrix" environment
## 2. The inverse matrix return objects are made invisible to not overwhelm the screen and
## to better isolate the computation time
## 3. The 2000x2000 matrices take about 16 seconds on my computer to compute the inverse
## So getting the cached copy of the inverse (near instant) is much faster

## The makeCacheMatrix function is a "CacheMatrix" wrapper for our matrix

makeCacheMatrix <- function(x = matrix()) {
    
        # Initialized inverse matrix cachedInverse is NULL
        cachedInverse <- NULL
    
        # Function to set the "CacheMatrix" object
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
    
        # Function to retrieve the original matrix
        get <- function() x
    
        # Function to set the inverse matrix to cachedInverse (propagated up one scope level)
        setinv <- function(computedInverse) cachedInverse <<- computedInverse
        
        # Function to get the cachedInverse
        getinv <- function() invisible(cachedInverse)
        
        # Return list of functions
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## The cacheSolve function applied to a "CacheMatrix" returns the matrix inverse, either by
## computing it (the first time it is run), or retrieving it from the cached copy

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        cachedInverse <- x$getinv()
        if(!is.null(cachedInverse)) {
            message("getting cached Inverse")
        return(invisible(cachedInverse))
        }
        
        myMatrix <- x$get()
        computedInverse <- solve(myMatrix, ...)
        x$setinv(computedInverse)
        invisible(computedInverse)
}


