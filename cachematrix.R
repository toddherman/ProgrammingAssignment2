# These two functions are used to create an
# object that stores a "matrix" and caches its inverse.

# This function creates a "matrix" object that can cache its inverse.
# It creates a "vector", which is really a list containing a function 
# to set and get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The following function calculates the inverse of the "matrix"
# created with the function above. It first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {  # if it already exists, return the cached value
        message("getting cached data")
        return(m)
    }
    data <- x$get() #it was not cached, so get the matrix
    m <- solve(data, ...) # calculate its inverse
    x$setinverse(m) #commit the inverse to the cache
    m
}
