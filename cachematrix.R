## The function makeCacheMatrix is used as the cache matrix, containing the
## inverse of a given matrix. Then cacheSolve uses the results of
## makeCacheMatrix to get the inverse of a given matrix. If the inverse has not
## been calculated, then it calculates it for the first time, but if it has
## been calculated previously then it simply returns the cached version.

## makeCacheMatrix creates a special list of functions:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

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


## cacheSolve calculates the inverse of the special "matrix" created with the
## above function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the
## inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s ## Return a matrix that is the inverse of 'x'
}