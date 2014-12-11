## Creates a pair of functions that return the inverse of a matrix. If the inverse
## of the matrix has been calculated before, it is pulled from a cache, rather
## than recalculating.

## Returns a list of functions that set and return the original matrix and
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}

	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Calculates the inverse of the original matrix. Takes the list created
## by makeCacheMatrix as the primary argument (x) and returns the inverse
## of the matrix input into makeCacheMatrix.

cacheSolve <- function(x, ...) {
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
