## Cache the calculated inverse of a matrix the first time it is calculated and 
## returns the cached value when the matrix ix not changed

## This function returns a list of functions to handle the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<-y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<-inverse
    getinverse <- function()inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}


## This function returns a the inverse of matrix
## but also caches the value the first time it is calculated
## if the matrix is not changed the the inverse is requested again
## the cached value is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
