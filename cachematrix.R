## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

## The function makeCacheMatrix creates a special "matrix" that sets and gets 
## the value of the matrix, and it also sets and gets the value of the inverse 
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## Write a short comment describing this function

## The function cacheSolve calculates the inverse of the matrix created 
## by makeCacheMatrix. But first it checks to see if the inverse was 
## already calculated, if yes then it just gets the mean from the cache 
##and skips the calculation. Otherwise it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via the 
## setInverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

