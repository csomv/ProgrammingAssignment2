## Caching the inverse of a matrix based on provided example code.
## It is assumed that the matrix supplied is always invertible.


## makeCacheMatrix function creates a special "matrix" class object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	set <- function(y) {
				x <<- y
				m <<- NULL
		}
	get <- function() x
	setinvm <- function(invm) m <<- invm
	getinvm <- function() m
	list(set = set, get = get,
		 setinvm = setinvm,
		 getinvm = getinvm)
}


## cacheSolve function computes the inverse of the matrix returned by
## makeCacheMatrix function above. If the inverse has already been calculated 
## (and the matrix has not changed), then this function will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	m <- x$getinvm()
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinvm(m)
	m
}
