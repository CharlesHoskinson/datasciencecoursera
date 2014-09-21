# Charles Hoskinson; 09/21/2014
# This file contain two functions. One that generates a data structure, which
# contains four elements. Second a function to compute or return an inverse of
# a matrix

# takes a given matrix and return a data structure containing the logic 
# necessary to store and mutate the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL # Initialize the storage matrix
	#set the cache matrix to a given argument y 
	set <- function(y) {
		x <<- y
		m <<- NULL #if set, then we shouldn't have an inverse value yet
	}
	get <- function() x #returns the matrix x
	setInverse <- function(inverse) m <<- inverse #sets and caches inverse 
	getInverse <- function() m #returns cached inverse
	list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)
	
}


## cacheSolve loads a matrix X and then checks if it already has solved
## the inverse. If the inverse has already been solve, then the function
## will return the value else it will compute and store the solution in x

cacheSolve <- function(x, ...) {
	m <- x$getInverse() 
	#Check if already solved
	if(!is.null(m)){ #inverse exists and is retreived 
		message("Getting Cached Inverse")
		return(m) 
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	return(m)	
}
