## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object. It's a list object containing:
# 1 set the value of the vector
# 2 get the value of the vector
# 3 set the value of the inverse
# 4 get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solv) s <<- solv
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve calculate the inverse of a special "matrix" object created with the above function.
## It first checks if the inverse has already been calculated. If so, it gets the inverse from the cache
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s   ## Return a matrix that is the inverse of 'x'
}
