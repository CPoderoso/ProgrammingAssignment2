## These functions try to use cached data from an inverse matrix 
## They are not different from the ones given by example on this
##  Assignment
## Functions:
## # makeCacheMatrix <- function(x = matrix())
## #  Parameter: matrix
## #  Returns: list
## #
## # cacheSolve <- function(x, ...)
## #  Parameter: list (probably from makeCacheMatrix)
## #  Returns: inverse matrix
## #           can return a message that cache was used

## This function will get the matrix and create a 'vector' that seems to
##  me much like an object.
## The methods are:
##  set, get, setsolve and getsolve

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


## This function will get the 'makeCacheMatrix' return and will check
##  if it is in cache.
## If the matrix is in cache, it will use the cached version. If it is not
##  it will ask to invert the matrix.

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
