## This file defines two functions that allow for calculation and caching
## of the inverse of a matrix.
## Example of usage:
##		matrixHolder <- makeCacheMatrix(matrix(runif(2000*2000),nrow=2000,ncol=2000))
##		## Slow
##		matrixInverse <- cacheSolve(matrixHolder)
##		## Fast
##		matrixInverse <- cacheSolve(matrixHolder)
## Subsequent calls to cacheSolve for this matrixHolder will be very fast until
## the matrix in the environment is set to a different matrix:
##		matrixHolder$set(matrix(rnorm(2000*2000),nrow=2000,ncol=2000))
##		## Slow
##		matrixInverse <- cacheSolve(matrixHolder)
##		## Fast
##		matrixInverse <- cacheSolve(matrixHolder)

## * makeCacheMatrix: This function takes a matrix and returns a list 
##		data structure whose elements are themselves functions that allow
##		for getting and setting a matrix and the inverse of the matrix.
##		The functions defined in the list have in their environment the
##		matrix whose invsere is to be cached.
makeCacheMatrix <- function(x = matrix()) {
	# Initially the cache is empty
	m <- NULL

	# Define the getters and setters for the held matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x

	# Define the getters and setters for the inverse (cached value)
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m

	# Return the matrix holder itself, which is the list and the environment
	# defined in this function that holds the matrix
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

## * cacheSolve: This function takes the list data structure returned
##		by makeCacheMatrix and returns the inverse of the matrix owned
##		by the environment of that list.
cacheSolve <- function(x, ...) {
	# If inverse is already cached, just return that.
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	# Otherwise get the matrix from the environment, calculate the inverse, and...
	data <- x$get()
	m <- solve(data, ...)
	# ...cache it in the environment of the argument
	x$setinverse(m)

	# return the inverse
	m
}
