## Following functions define a matrix "object", makeCacheMatrix,
## and a wrapper function, cacheSolve, which returns the cached matrix inversion
## if one was previously computed

## makeCacheMatrix: function to provide cache for matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL # initialize cached inverse to null
    
    # set inv variable of parent function
    setinv <- function(i) inv <<- i
    
    # return inv variable
    getinv <- function() inv
    
    # return matrix
    get <- function() x
    
    # set matrix variable in parent and invalidate cached inverse
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    
    # return list of functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: returns cached inverse of matrix if already computed

cacheSolve <- function(x) {
    
    # get the cached value
    x.inv <- x$getinv()
    if(!is.null(x.inv) ) {
        message("Returning cached inverse")
        return(x.inv)
    }
    
    # not cached, compute and set
    x.inv <- solve(x$get())
    x$setinv(x.inv)
    
    # return inverse
    x.inv
}
