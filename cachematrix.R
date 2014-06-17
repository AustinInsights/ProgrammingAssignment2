## Put comments here that give an overall description of what your
## functions do

## ---------------------------------------------------------------
## This function, makeCacheMatrix creates a special "matrix" object that can cache its inverse
## returns a list containing a function to
## 
## set a matrix
## get a matrix
## set the inverse of the matrix
## get the inverse of the matrix
## 
## ---------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
		matrixInverse <- NULL
		
		set <- function(y) {
			x <<- y
			matrixInverse <<- NULL
		}
		
		get <- function() {
			x
		}
		
		setinverse <- function(inverse) {
			matrixInverse <<- inverse
		}
		
		getinverse <- function() {
			matrixInverse
		}
		
		list(set = set, get = get,
				setinverse = setinverse,
				getinverse = getinverse)
}


## ---------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.
##
## IMPORTANT: Assumes that the matrix supplied is always invertible
## The following error is thrown if the matrix supplied is not invertible, for example non-square
##
## *** Error in solve.default(originalMatrix, ...) : 'a' (8 x 2) must be square ***
##
## ---------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		matrixInverse <- x$getinverse()
		
		if(!is.null(matrixInverse)) {
			message("getting cached inverse")
			return(matrixInverse)
		}
		
		originalMatrix <- x$get()
		
		matrixInverse <- solve(originalMatrix, ...)
		
		x$setinverse(matrixInverse)
		
		matrixInverse
}
