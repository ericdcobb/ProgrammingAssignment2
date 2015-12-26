## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
