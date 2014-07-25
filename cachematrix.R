## R Programming: Assignment 2: Caching the Inverse of a Matrix
##
## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse.
##
## For this assignment, we assume that the matrix is always invertible.


## This function creates a matrix object that can store its inverse. It
## requires one input parameter x that is a square numeric matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
        ## This function returns a list of action calls.
}


## This function computes the inverse of a special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix is unchanged), then cacheSolve retrieves the cached inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## This function returns a matrix that is the inverse of x.
}
