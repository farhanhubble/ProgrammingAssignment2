## Fast(cached) matrix inversion.
#
## makeCacheMatrix() stores the inverse of a matrix and provides a list 
## functions to access and update the matrix or its inverse.
#
## cacheSolve() inverts a matrix given a list function to access and update
## the matrix and its inverse. If it finds a non NULL inverse, it returns 
## that, otherwise it computes the inverse anew and caches it for future use.


 

## makeCacheMatrix(x=matrix()) : Given a matrix (invertible) 'x', this function
## returns a list of 'accessor' functions that can be used to get or set the 
## matrix 'x'and its inverse 'invx'. 

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    
    set <- function(y) {
        x <<- y
    }
    
    get <- function() {
        x
    }
    
    setInv <- function(inv) {
        invx <<- inv
    }
    
    getInv <- function() {
        invx
    }
    
    ## Return a list of the four methods defined above.
    list(set=set, get=get, setInv=setInv, getInv=getInv)

}


## cacheSolve(x,...) : Given 'x' - a list of accessors functions, of some
## matrix 'M', returns the cached inverse of 'M' if it has been already
## computed or computes the inverse afresh, caches it then returns it.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Return cached inverse if it exists.
    inv <- x$getInv()
    if(!is.null(inv)) {
        return(inv)
    }
    
    # Compute new inverse, set cached inverse 
    # and return freshly computed inverse. 
    inv <- solve(x$get())
    x$setInv(inv)
    inv
}


