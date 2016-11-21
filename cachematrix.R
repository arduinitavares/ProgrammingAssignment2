## These functions are able to cache potentially time-consuming computations.
## Taking the inverse of a numeric matrix is typically a fast operation.
## However, for a very long matrix, it may take too long to compute the mean,
## especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a matrix are not changing, it may make sense to cache
## the value of the mean so that when we need it again, it can be looked up 
## in the cache rather than recomputed

## This function creates a special "matrix", which is a list containing a function to:
# -set the value of the vector;
# -get the value of the vector;
# -set the value of the mean;
# -get the value of the mean;

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the mean of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        message("calculating cached data")
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        x
}
