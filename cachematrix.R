## The methods below provide the end user the benefit of caching
## while performing the inverse of a matrix

## The function 'makeCacheMatrix' does:
## sets the matrix to be inversed
## gets the matrix to be inversed
## sets the inversed matrix
## gets the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
 	  ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinv <- function(solve) ix <<- solve
        getinv <- function() ix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The 'cacheSolve' function calculates the inverse of the matrix
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  ix <- x$getinv()	  
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinv(ix)
        ix
}
