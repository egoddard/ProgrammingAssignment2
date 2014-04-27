## Calculates the inverse of a matrix. If the inverse has already been
## calculated, a cached inverse is returned.

makeCacheMatrix <- function(x = matrix()) {
    ## Returns a list containing functions to get and set the matrix and its
    ## inverse.
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Print the matrix
    get <- function() x
    
    ## Updates the inverse of the matrix
    setInverse <- function(inverse) i <<- inverse
    
    ## Prints the inverse of the matrix
    getInverse <- function() i
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    ## Takes a MakeCacheMatrix list as argument and updates the inverse if it
    ## does not exist.
    
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
