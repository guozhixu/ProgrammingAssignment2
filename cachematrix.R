## For a very large matrix, it may take too long to compute the inverse,
## especially if it has to be computed repeatedly. If the contents of a matrix
## are not changed, it may make sense to cache the value of the inverse so that
## when we need it again, it can be looked up in the cache than recomputed.
##
## There are two functions here.
## "makeCacheMatrix" function creates a special "matrix" object that can cache
## its inverse.
## "cacheSolve" function calculates the inverse of the special "matrix" created
## with the "makeCacheMatrix" function.


## This function creates a special "matrix" object that can cache its inverse.
## This function is composed of 4 child functions:
## 1. set(): set the value of the matrix
## 2. get(): get the value of the matrix
## 3. setsolve(): set the value of the inverse of matrix
## 4. getsolve(): get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
			 setsolve = setsolve,
			 getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
