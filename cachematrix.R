## The methods below provide the end user the benefit of caching
## while performing the inverse of a matrix

## The function 'makeCacheMatrix' does:
## sets the matrix to be inversed
## gets the matrix to be inversed
## sets the inversed matrix
## gets the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
	  
	  # Initialize the Inverse Matrix variable to NULL
 	  ix <- NULL

	  # Set the value of the Input Matrix
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }

	  # Get the Value of the input Matrix
        get <- function() x

	  #Set the inversed Matrix
        setinv <- function(solve) ix <<- solve

	  #Get the inversed Matrix
        getinv <- function() ix

	  #List the set's and get's
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The 'cacheSolve' function calculates the inverse of the matrix
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value

cacheSolve <- function(x, ...) {
        
	  # Check the inverse in cache
	  ix <- x$getinv()	  
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }

	  # If the above condition is not true get the input matrix
        data <- x$get()

	  # Compute the inverse of the input matrix
        ix <- solve(data, ...)

	  #Set the inversed Matrix in cache
        x$setinv(ix)

	  #Return the inversed Matrix
        ix
}
