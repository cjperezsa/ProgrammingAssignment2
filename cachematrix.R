## makeCacheMatrix(matrix): makes a cacheable inverse of matrix
## and its inverse

## cacheSolve(cacheablematrix): gets the cached inverse of matrix
## or calculates it and sets it into the cacheable matrix object

## makeCacheMatrix mmakes a matrix and keep its inverse
## when it's calculated
## Arguments: x: a R matrix() object. We assume it's invertible
## Returns a list of 4 functions for getting the data,
## setting the data, getting the inverse and setting the inverse
## respectively
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    get <- function () x
    set <- function(y) {
        i <<- NULL
        x <<- y
    }
    getInverse <- function () i
    setInverse <- function(ii) { i <<- ii }
    
    list(get = get,
         set = set,
         getInverse = getInverse,
         setInverse = setInverse)

}


## cacheSolve(makeCacheMatrix) computes the inverse of a
## matrix and sets into it or, in case it has already been
## calculated, return the cached matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    i
}
