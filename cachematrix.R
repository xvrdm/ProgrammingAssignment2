## Put comments here that give an overall description of what your
## functions do

## These two functions help us create a special "matrix" object, which can store
## its inverse in cache. This could be useful in scenario where you need to 
## use the inverse of a matrix many times and do not want to use a lot of cpu 
## power every time.

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix", which is really a list containing functions
## to get/set the value of the matrix and get/set the value of this matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
	matInv <- NULL
	
	## set the value of the matrix
	set <- function(y) {
		x <<- y
		matInv <<- NULL
	}
	
	## get the value of the matrix
	get <- function() x
	
	## set the value of the inverse of the matrix
	setInv <- function(inv) matInv <<- inv
	
	## get the value of the inverse of the matrix
	getInv <- function() matInv
	
	## return the new list object
	list(set = set, 
	     get = get,
	     setInv = setInv,
	     getInv = getInv)
}

## Write a short comment describing this function

## cacheSolve returns the matrix's inverse from cache if it has already been
## calculated. If not, it calculates the inverse, save it in cache and returns it.

cacheSolve <- function(x, ...) {
	## get the x matrix's inverse from cache
	matInv <- x$getInv()
	
	## if the inverse is not null, return it
	if(!isnull(matInv)) {
		message("getting cache data")
		return(matInv)
	}

	## if the inverse was null,
	data <- x$get() ## get the matrix data
	matInv <- solve(data,...) ## calculate the inverse 
	x$setInv(matInv) ## save the inverse in cache
	matInv ## return the inverse
}
