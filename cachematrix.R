## The makeCacheMatrix function creates a special matrix object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        Inv <- NULL

	## Set the matrix and its inverse
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }

	## Return matrix
        get <- function() x
	
	## Set the inverse of matrix
        setInverse <- function(Inverse) Inv <<- Inverse

	## Return the inverse of matrix
        getInverse <- function() Inv 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The cacheSolve function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then  cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    
	Inv <- x$getInverse()

	## Check if the inverse has already been calculated and stored in cache
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }

        data <- x$get()
	## If not calculated previously, calculate and return a matrix that is the inverse of 'x'
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv
}
