## Put comments here that give an overall description of what your
## functions do

# This functions keep matrix and compute inverse matrix.
# Input is matrix.
# Output is list of functions to get matrix and compute inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

# This function is computing inverse matrix.
# Input for these function is list of functions created by previous function.
# In case if inverse matrix was already computed, this function returns it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

# Example
# m = rbind(c(1, -1/4), c(-1/4, 1))
# l <- makeCacheMatrix(m)
# cacheSolve(l)

