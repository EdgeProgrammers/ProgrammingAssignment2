## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a list containing a function to:
##          (i)   Set the matrix 
##          (ii)  Get the matrix
##          (iii) Set the inverse of matrix
##          (iv)  Get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setmatrix <- function(y){
        x <<- y
        inverse <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
    
} 


## This function solves the inverse of the special matrix 
## created by the above function. In case, inverse is already 
## calculated, it gets it from the cache and prevent further
## computation. otherwise, it uses solve(data) to get the inverse
## of the matrix.   

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$getmatrix()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse       ## Return a matrix that is the inverse of 'x'
    
}



