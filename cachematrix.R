## These functions calculate the inverse of matrix x and cache it in the global
## environment.

## This function stores a matrix x in the cache, clears any value of s that had
## been cached by a prior execution of cacheSolve(), and defines and names
## getting and setting functions to be used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function calculates s (the inverse of matrix x) and caches it in the 
## global environent. It also first checks to see if a cached value of s is
## already present in the global environment; if there is, the function will 
## return the message "getting cached data", meaning that no new matrix x has
## been passed through makeCacheMatrix().

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}