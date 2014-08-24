# R Programming Assignment 2

## The following two functions cache the inverse of a matrix.
## Reasoning: Matrix inversion can be costly (both time and resource-wise),
## especially if matrices are big. It can thus be valuable to cache the
## results of a matrix inversion instead of computing the inverse
## every single time.


### The function makeCacheMatrix creates a special "matrix" object that
### can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    #initialize matrix object
    m <- NULL
    
    #set the matrix
    setMatrix <- function(new){
        x <<- new
        m <<- NULL
    }
    
    #get the matrix
    getMatrix <- function(){x} #returns the matrix
    
    #set the inverse of the matrix
    setInverse <- function(inverse)  m <<- inverse
    
    #get the inverse of the matrix
    getInverse <- function() m
    
    #return the list of methods
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}



### The function cacheSolve computes the inverse of the special "matrix"
### returned by makeCacheMatrix above. If the inverse has already been
### calculated (and the matrix has not changed), then the cachesolve
### should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    #return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    #returns inverse if it was cached and prints it out
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #get the matrix
    data <- x$getMatrix()
    
    #calculate the inverse of the matrix
    m <- solve(data)
    
    #set the inverse
    x$setInverse(m)
    
    #return the matrix
    m
}


