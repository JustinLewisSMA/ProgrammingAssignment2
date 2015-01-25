## The following functions cache the inverse of a matrix
# Example usage:
# > y <- makeCacheMatrix(matrix(c(3, -7, 5, 2), 2, 2))
# > y$get()
#      [,1] [,2]
# [1,]    3    5
# [2,]   -7    2
# > cacheSolve(y)
#            [,1]        [,2]
# [1,] 0.04878049 -0.12195122
# [2,] 0.17073171  0.07317073
# 
# > y$getinverse()
#            [,1]        [,2]
# [1,] 0.04878049 -0.12195122
# [2,] 0.17073171  0.07317073
#
#
# This function creates a special matrix-like object that can cache its inverse
# * This function does not check to see if a matrix is invertable
makeCacheMatrix <- function(x = matrix()) {
        
        # m is a marker to see if a cache has been set.
        m <- NULL
        
        # This function creates the matrix.
        # We also reset the marker so we can tell if the cache has been set.
        set <- function(y) {
                # We use the <<- operator to set x and m on the parent
                x <<- y
                m <<- NULL
        }
        # This function simply returns the value that was set.
        get <- function() x
        
        # This function is used by cacheSolve to set the cache
        # This also sets our marker m, so we know if the inverse is cached
        setinverse <- function(solve) m <<- solve
        
        # This function returns the cached inverse, if it exists.
        getinverse <- function() m
        
        # This is simply returning the methods for our mmakeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function computes the inverse of the special matrix-like object returned 
# by makeCacheMatrix. If the inverse has already been calcuated, 
# it returns the previously calculated result
cacheSolve <- function(x, ...) {
        
        # Check to see if we have an inverse already set
        m <- x$getinverse()
        
        # If the inverse is already set, return the cached inverse
        if(!is.null(m)) {
                message("Getting cached data.")
                return(m)
        }
        
        # inverse is not set. Get the data from x
        data <- x$get()
        
        # determine the inverse
        m <- solve(data, ...)
        
        # set the inverse on x
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
