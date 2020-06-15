## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix object that can cache its inverse
##it creates a list setting the value of the list and the value of the matrix
##inverse, saving these values if they are needed

makeCacheMatrix <- function(x = matrix()) {

	s <- NULL
	set <- function (y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


##this function computes the inverse of the special matrix returned by makeCache
##Matrix. If the inverse has already been calculated and the matrix has not
##changed, then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
