## The below pair of functions cache the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed),
## the cached data is returned; else - a newly calculated inverse is returned.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## @x: a square invertible matrix    
    
    # initialize to NULL
    inv <- NULL
    
    # create matrix in the working environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get matrix value
    get <- function() x
    
    # invert matrix and store in cache
    setinv <- function(solve) inv <<- solve
    
    # get inverted matrix from cache
    getinv <- function() inv
    
    # return functions to the working environment
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix

cacheSolve <- function(x = matrix(), ...) {
    ## @x: output of makeCacheMatrix()
    ## Return a matrix that is the inverse of 'x'

    ## try to get the inversed matrix from cache
    inv <- x$getinv()
    
    ## return inverted matrix from cache if it exists, else - calculate
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## since no invert exists in cache - calculate the invert
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv    
}
