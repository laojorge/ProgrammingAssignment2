## This program calculates the inverse of a matrix and places the results in cache

## This function write a inverse matrix in cache
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(inversematrix) m <<- inversematrix
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}

## This function calculates the inverse matrix
cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        message("Calculating ...")
        m <- solve(data, ...)
        x$setinversematrix(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
