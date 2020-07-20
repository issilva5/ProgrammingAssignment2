## Put comments here that give an overall description of what your
## functions do

## This function implements a cached matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Calculates the inverse matrix of the input, using cache if it has been calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
        	inv
        }
        
        data <- x$get()
        resp <- solve(data, ...)
        x$setinv(resp)
        resp
}
