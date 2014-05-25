## Put comments here that give an overall description of what your
## functions do

## COMMENTING ON FIRST FUNCTION
## First function makeCacheMatrix will create a function (which will be better to refer as an
## environment in this example), where the initial matrix will be stored and where the four 
## other functions will be defined - these functions are the methods allowing the basic 
## manipulations with the given matrix, stored in this environment.
## The functions are:
##      * set(y) - assigns a matrix given in 'y' to the matrix 'x' stored in this environment
##      * get() - returns the matrix 'x' stored in this environment
##      * setInverse(invMatrix) - stores the matrix given in 'invMatrix' in the variable 'inv'
##                  created in this environment, assuming it is the inverse matrix to matrix 'x'solve
##      * getInverse() - returns the 'inv' matrix stored in this environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(invMatrix) inv <<- invMatrix
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## COMMENTING ON SECOND FUNCTION
## Second function takes the makeCacheMatrix object as an argument, gets the inverse matrix
## from that object, stored in 'inv' variable via its environment, then checks if the 'inv'
## variable is not empty. 
## If the inverse matrix has been already calculated and stored in 'inv', the function simply
## returns the 'inv' matrix, else it calculated the invMatrix, stores it in the given object 'x'
## and finally returns 'inv' value calculated at the moment.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        print("Getting cached matrix...")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setInverse(inv)
    inv
}

## USING THESE FUNCTIONS
## For example we have a matrix 'm' and we are going to refer to it a lot in terms of calculating
## its inverse matrix. We are creating an object 'mMatrix' initializing it with makeCacheMatrix(m).
## > nMatrix <- makeCacheMatrix(m)
## Then when we need to calculate the inverse matrix of 'm' we are calling it with cacheSolve(mMatrix)
## instead of solve(m) and that will save us some very precious time when dealing with large matrices.
## 
## cacheSolve(mMatrix) is better than solve(m), bro!
