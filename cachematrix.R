## Caching the inverse of a Matrix

## makeCacheMatrix creates a speical "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inputinverse) inverse <<- inputinverse
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes the inverse of the sepcial "matrix" returned by the
## function above. If the inverse has already been calculated, then the function
## should retrieve the inverse from cache.

cacheSolve <- function(x, ...){
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
}
