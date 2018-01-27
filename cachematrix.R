## Allow matrix inversion results to be cached so that the inverted matrix is calculated  
## the first time but thereafter the original results are returned.

makeCacheMatrix <- function(x = matrix()) {
	## Wraps a matrix in an object that allows the first inversion result to be
	## cached so that inversion is not repeated.
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


cacheSolve <- function(x, ...) {
	## Inverts a matrix created by makeCacheMatrix() using the cached
	## inverted matrix after it has been inverted the first time.

	## Test Case: 	m <- matrix(c(4, 2, 7, 6), 2, 2)
	## 		cm <- makeCacheMatrix(m)
	##		cacheSolve(cm)
	## Expected:	 0.6 -0.7
	##		-0.2  0.4
	## Inverted:	m %*% cm = I (identity matrix)

	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
