## makeCacheMatrix creates a special matrix object that can cache
## its inverse
## cacheSolve computes the inverse of the special matrix

## makeCacheMatrix has three functions. The function 'set' will set the value
## of the matrix. The function 'get' will return the value of the matrix.
## The function 'setinverse' will inverse the matrix. The function 'getinverse'
## will return the interse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(y) m <<- y
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function cacheSolve will return inverse matrix from cache if it exists,
## otherwise it will do inversion

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

