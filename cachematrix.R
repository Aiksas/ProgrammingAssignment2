## This functions together will calculate the inverse
## of a matrix, and store the result in the cache. 
## Therefore, if the result is needed later, it can simply
## be retrieved from the cache, instead of having
## to be recalculated.

## this function will create a special matrix that 
## is underlyingly a list containing a function to 
## set the value of the matrix, get the value of that 
## matrix, set the value of the inverse, and get the
## value of that inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix()
    set <- function(y) {
        x <<- y
        m <<- matrix()
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function will calculate the inverse of 
## the matrix given, but only when that value
## has not been calculated yet - otherwise it'll
## get that value from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message('getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
