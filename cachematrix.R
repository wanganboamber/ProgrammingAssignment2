## Put comments here that give an overall description of what your
## functions do

## These functions can cache the inverse of a matrix


## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix"
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_input) inverse <<- inverse_input
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse
             getinverse = getinverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the
## special "matrix" created with the above function
## It first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via
## the setinverse function

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}
