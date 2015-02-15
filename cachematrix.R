## These functions cache the inverse of a matrix so it can be
## looked up rather than recomputed each time. 

## This function creates a list that contains a function which
## 1) sets the value of the matrix
## 2) gets the value of the matrix
## 3) sets the value of the inverse
## 4) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## The following function calculates the inverse of the matrix
## created with the above function. It first checks to see if
## the inverse has already been calculated. If so it gets the
## inverse from the cache and skips the computation. Otherwise
## it calculates the inverse of the matrix and sets the
## inverse in the cache via the setmatrix function

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
    
}
