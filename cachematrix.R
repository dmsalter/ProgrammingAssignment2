## R Programming: Assignment 2: Caching the Inverse of a Matrix
##
## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse.
##
## For this assignment, we assume that the matrix is always invertible.


## The makeCacheMatrix function creates a matrix object that can store its
## inverse. It requires one input parameter x that is a square numeric matrix.
makeCacheMatrix <- function(x = matrix()) {
	# Initialize the inverse matrix m to NULL.
	m <- NULL
	# Set the matrix x to input y and intiailize the inverse matrix m.
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	# Retrieve the original input matrix x to check what is stored.
	get <- function() x
	# Calculate the inverse of the stored matrix x and save that to matrix m.
	setinverse <- function(solve) m <<- solve
	# Retrieve the cached inverse matrix m.
	getinverse <- function() m
	# Save these action items to a list.
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
# This function returns a list of action calls that are defined here.
}


## The cacheSolve function computes the inverse of a special matrix returned
## by makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix is unchanged), then cacheSolve retrieves the cached inverse.
cacheSolve <- function(x, ...) {
	# Set m to the inverse matrix of x by calling the getinverse list item.
        m <- x$getinverse()
        # If m is not NULL, then return the cached matrix m and exit cacheSolve.
        if(!is.null(m)) {
                message("Getting cached data...")
                return(m)
        }
        # If m is NULL, then set the data matrix to the stored original input x.
        data <- x$get()
        # Solve for the inverse of x and store it in matrix m.
        m <- solve(data, ...)
        # Save this solution to cache, so that you do not need to repeat it.
        x$setinverse(m)
        # Return the inverse of input x.
        m
## This function returns a matrix that is the inverse of x.
}
