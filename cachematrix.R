## These two functions calculate and chache the time consuming operation
## of inverting a matrix so if the data does not change and the inversion function 
## is called again the previously calculated and cached value is returned

# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matirx
# set the value of the matrix inversion
# get the value of the matirx inversion

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) m <<- invert
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


#Calculates the matrix inversion for the list object created with the above function. 
#First check to see if the inversion has already been calculated, and return cached value.
# Otherwise calculate the inversion of the matrix and sets the the cache via the setinvert function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}
