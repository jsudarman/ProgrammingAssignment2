## There are 2 functions in this document
## 1. First function is for creating matrix and 
## calculate its inversion
## 2. Second function is for execution performance,
## it will cache the result and use it whenever possible
## We are using R function 'solve' to get matrix inversion
## We assume that matrix will always be square invertible

## makeCacheMatrix
## function to find matrix inversion
## we assume that matrix() is square matrix invertible

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
			x <<- y
			i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list (set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}


## cacheSolve
## function to check whether inverse matrix is cached
## if not calculate the inversion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
        		message("getting cached data")
        		return(i)
        }
        data <- x$get()
        i <- solve(i)
        x$setinverse(i)
        i
}
