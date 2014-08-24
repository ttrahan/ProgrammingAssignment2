## This code eliminates the need to compute the inverse of a matrix repeatedly 
## if the matrix object hasn't changed since the first time the inverse was 
## computed.

## This function will add add additional functionality/meta data to a matrix 
## the user passes to the function.  This will result in the ability to do one
## four activities - set a new value for the inputted matrix, get the value of
## the matrix currently passed to the function, set/cache the value of the
## inverse for this matrix, and get/retrieve the value of the inverse if it has 
## already been calculated and stored.  Note that calculation of the inverse does
## not occur in this function, but in the calling function "cacheSolve".

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {  ## replace the matrix with a new matrix y
                x <<- y       ## assign y to X inside and outside this function
                i <<- NULL
        }
        get <- function() {x} ## return the matrix when called with $get()
        setinverse <- function(inverse) {i <<- inverse}  ## cache the inverse as 
                ## calculated in the calling function
        getinverse <- function() {i}  ## return the cached inverse when called
                ## with $getinverse()
        list(set = set, get = get,  ## store all functions in a list
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will calculate the inverse of a matrix, store the value of the 
## inverse, and retrieve the stored value of the inverse instead of recalculating
## the inverse if the matrix passed to the function hasn't changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()  ## get the cached inverse
        if(!is.null(i)) {  ## if the inverse is cached, retrieve it
                message("getting cached data")
                return(i)  ## end function execution
        }
        data <- x$get()  ## retrieve matrix to use for inverse calcuation
        i <- solve(data) ## calculate the inverse
        x$setinverse(i)  ## cache the inverse by calling setinverse() in the 
                         ## makeCacheMatrix function
        i                ## return the value of the inverse
}
