## Put comments here that give an overall description of what your
## functions do

##
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
            x <<- y
            s <<- NULL
    }
    get <- function() x
    setinv <- function(inv) s <<- inv
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}
