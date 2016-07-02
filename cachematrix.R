### Below are two functions that are used to create a special object
## that stores a numeric matrix and cache's its inverse matrix.

## The makeCacheMatrix function creates a list containing
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function calculates the inverse of a matrix created
## with the above function. Before calculating the inverse, it first
## checks if the inverse has already been calculated. If so, it
## gets the inverse from the cache and skips the computation. If not,
## it calculates the inverse matrix and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
