## makeCacheMatrix creates a cache of the inverse of a matrix while the second will try to retrived the cached inverse, and if it does not exist will calculate it

## Calculates the inverse of a matrix and caches it

makeCacheMatrix <- function(x = matrix()) {
i<- NULL
	set<- function(y) {
		x<<- y
		i<<- NULL
	}
	get<- function() x
	setinv<- function(solve) i<<- solve
	getinv<- function() i
	list(set=set, get=get,
	setinv=setinv,
	getinv=getinv)
}


## Calculates the inverse of the matrix, but first checks if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data<- x$get()
	i<- solve(data, ...)
	x$setinv(i)
	i
}
