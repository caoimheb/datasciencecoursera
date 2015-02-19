## The functions below will calculate and cache the inverse of a matrix. 
## If the matrix inverse is to be calculated again, but the matrix
## has not changed, then the cached inverse will be returned.

## This function creates a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the "matrix" returned in makeCacheMatrix().
## If the inverse has already been calculated, and the matrix has not changed, 
## then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  
}
