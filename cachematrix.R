###############################################################################
##   Date: 18 August 2014
##  Title: HW Assignment 2
## Course: Coursera R-Programming
## Author: Seth Guberman
## ----------------------------------------------------------------------------
## These functions demonstrate the use of R's scoping rules to cache a
## computationally expensive result for later retrieval (e.g., in a loop).
## A constructor function is used to create a special 'matrix' object on
## which the solver operates.  In this case, the inverse of a matrix is
## computed, but this concept could easily be applied to other operations.
## Valid input is assumed.
###############################################################################

## This function creates a 'matrix' object (really just a list of functions)
## which has the ability to 'remember' or cache the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    # start with the inverse, i, as NULL
    # this is the value that gets cached once computed
    i <- NULL
    
    # define the set function
    set <- function(y) {
        # take a value, y, and assign it to x, our matrix
        # double-arrow is used to assign outside the current environment
        x <<- y
        # reset the inverse, i, since the matrix has now changed
        i <<- NULL # double-arrow again
    }
    
    # define the get function, which simply returns x, the matrix
    get <- function() x
    
    # define the setinverse function,
    # which simply redefines i to be the value of the argument passed
    setinverse <- function(inverse) i <<- inverse # double-arrow
    
    # define the getinverse function, which simply returns i, the inverse
    getinverse <- function() i
    
    # this is what gets returned (a list of the above helper-functions)
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function operates on the 'matrix' object above via the helper-functions
## defined within.  It returns a matrix that is the inverse of 'x', saving time
## if it has already been calculated and the input matrix has not changed.

cacheSolve <- function(x, ...) {
    # first, grab i from getinverse()
    # at this point it could either be solved already or have a NULL value
    i <- x$getinverse()
    
    # return i if it isn't NULL (it has been caluclated and x hasn't changed)
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i) # done! no calculation necessary, function exits here.
    }
    
    # otherwise...
    message("calculating new inverse")
    mat <- x$get() # get the matrix,
    i <- solve(mat, ...) # calculate the inverse using solve(),
    x$setinverse(i) # and set it in the cache to be used again.
    
    # finally, return i
    i
}
