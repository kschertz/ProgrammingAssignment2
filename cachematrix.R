## The first function creates a "matrix" object that can cache its inverse. The 
## second function computes the inverse of the matrix object returned by the
## first function. If the inverse is already calculated, it will retrieve the
## inverse from the cache instead of re-calculating.


## Creates "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i = NULL
	get <- function() { x }
	setinverse <- function(solve)
		{ i <<- solve}
	getinverse <- function() { i }
	list(get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns inverse of matrix. If already calculated, returns inverse from cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
