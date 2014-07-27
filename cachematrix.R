## cachematrix.R contains a system for storing matrices and their inverse
## Created in order to prevent the need to redo calculation 
##if it has been calculated previously

## Creates list of functions that will refer to created matrix.
## Returns functions: set, get, setinverse, and getinverse as list

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function (u) {
		x <<- u
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list( set =set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Finds inverse of the associated matrix, but first checks for stored value
## Returns the inverse of the matrix

cacheSolve <- function(x, ...) {        
	m <- x$getinverse()
	if(!is.null(m)) {
		message(“getting cached data”)
		return (m)
	}
	data <- x$get()
	m <- solve(data,…)
	x$setinverse (m)
	m
}
