## Functions that decorate a matrix with methods that allow caching its inverse.
## First, call `makeCacheMatrix` to create a cached matrix.
##Then, call `cacheSolve` on your cached matrix to get the inverse. 
##The inverse calculation will only be executed once.

## call `makeCacheMatrix` on a matrix to get a cached matrix.
## mat <- matrix( c(2, 4, 3, 1, 5, 7), nrow = 2, ncol = 2)
## cm <- makeCacheMatrix(mat)
##
## Then, you can call `cm$get()` to get the original matrix.
makeCacheMatrix <- function(storedMatrix = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        storedMatrix <<- y
        cachedInverse <<- NULL
    }
    get <- function() storedMatrix
    
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Invert a cachced matrix (created by the `makeCacheMatrix` function.)
## `cacheSolve(cm)`
cacheSolve <- function(cachedMatrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- cachedMatrix$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    message("No cached inverse found, calculating and storing...")
    storedMatrix <- cachedMatrix$get()
    inverse <- solve(storedMatrix, ...)
    cachedMatrix$setInverse(inverse)
    inverse
}
