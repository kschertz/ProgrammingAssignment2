## The first function creates a "matrix" object that can cache its inverse. 
## The second function computes the inverse of the matrix object returned by 
## first function. If the inverse is already calculated, it will retrieve the
## inverse from the cache instead of re-calculating.


## Creates "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i = NULL
	set <- function(y){				##Setting value of matrix
		x <<- y
		i <<- NULL
	}
	get <- function() { x }			##Getting value of matrix
	setinverse <- function(solve)
		{ i <<- solve}				##Setting value of inverse
	getinverse <- function() { i }	##Getting value of inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns inverse of matrix. If already calculated, returns inverse from cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {			##Checking if already calculated
        	message("getting cached data")
        	return(i)				##Returning stored inverse
        }
        data <- x$get()
        i <- solve(data, ...)		##If not already stored,solves inverse
        x$setinverse(i)				##Sets inverse and returns it
        i
}
