## Creates complex function with 4 functions inside
## Actual matrix inversion is absent in this function - it is performed by cachesolve function
## To use this function, assign it to variable with invertable matrix as argument

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set.i <- function(inv) i <<- inv
        get.i <- function() i
        list(set = set, get = get,
             set.i = set.i,
             get.i = get.i)
}

## This function takes makeCacheMatrix function as an argument

cachesolve <- function(f, ...) {
        i <- f$get.i()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- f$get()
        i <- solve(data, ...)
        f$set.i(i)
        i
}




