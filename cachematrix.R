## Creates a pair of functions that return the inverse of a matrix. If the inverse
## of the matrix has been calculated before, it is pulled from a cache, rather
## than recalculating.

## Returns a list of functions that set and return the original matrix and
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	## empty vector to hold inverse of matrix
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}

	## function holding matrix
	get <- function() x
	
	## function adding inverse to cache matrix
	setinv <- function(inverse) inv <<- inverse
	
	## function returning matrix inverse from cache
	getinv <- function() inv
	
	## return list of functions
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Calculates the inverse of the original matrix. Takes the list created
## by makeCacheMatrix as the primary argument (x) and returns the inverse
## of the matrix input into makeCacheMatrix.

cacheSolve <- function(x, ...) {

	## pull matrix inverse value from cache
        inv <- x$getinv()
        
        ## check if cache value exists; return from cache if it does
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## if not in cache - load matrix and calculate inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        ## add inverse to cache, and return inverse
        x$setinv(inv)
        inv
}
