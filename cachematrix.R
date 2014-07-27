## makeCacheMatrix returns a matrix bject which is a list having various attributes for setting, getting matrix and its inverse
## These object properties are used in cacheSolve function to calculate the inverse or cache it from the environment

## @params x the initial matrix which is used to generate the special object, default as empty matrix
## @return list with set of properties to set, get, setinverse amd getinverse
## @description - set function sets the value of the internal variable to the matrix passed and makes the value of inverse as NULL, since the actual data is changed
##		    - get returns the matrix
##		    - setinverse, sets the inverse in the object environment
##		    - getinverse, returns the inverse value of matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}


## @params x, the special object constructed using makecachematrix
##	     ..., arguments passed in to the solve function
## @return inverse of the matrix
## check if the inverse of the matrix is cached in the special object, if found, it returns it, else it calculates and cache it to the object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("inverse exist in cache for the data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setinverse(inv)
	inv
}
