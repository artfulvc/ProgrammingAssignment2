## Here you find two functions modeled on the functions provided
## in the assignment (makeVector() and cacheMean()). They extend the model
## of caching by taking a matrix as argument and using the solve() function
## instead of the mean() function.

## makeCacheMatrix takes a matrix, 'x', as its argument, and creates an empty 'cache'
## Inside the function are 4 interior functions: set, get, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    
    ## set takes as its argument the matrix 'y', and sets the value 
    ## of 'x' to be 'y' in the parent environment; it also sets the cache
    ## to be NULL in the parent environment.
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    ## get takes no arguments and returns a matrix, x.
    get <- function() {
        return(x)
    }
    
    ## setInverse takes a matrix as its argument and puts that matrix in the
    ## parent environment's cache.
    setInverse <- function(inverse) {
        cache <<- inverse
    }
    
    ## getInverse returns the cache matrix.
    getInverse <- function() {
        return(cache)
    }
    
    ## Return list of methods/functions
    list(set = set, get = get,
        setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve, like cachemean, takes a matrix, 'x', as its argument and
## returns a cached matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        
        ## get the cache and put it in 'cache'.
        cache <- x$getInverse()
        
        ## if the cache is empty, let the user know and then return 'cache'.
        if(!is.null(cache))  {
            message("Getting cached data...")
            return(cache)
        }
        
        ## get the data, find the inverse and put it in the cache
        ## return 'cache'.
        data <- x$get()
        cache <- solve(data, ...)
        x$setInverse(cache)
        return(cache)
}

## Testing script - you can toggle this on and off to test this code.
## Script creates a matrix, finds its inverse, then finds the inverse of that.
## The inverse of the inverse is same as the original.
# a <- matrix(1:4,2,2)
# a_cache <- makeCacheMatrix(a)
# b <- cacheSolve(a_cache)
# b_cache <- makeCacheMatrix(b)
# b_inverted <- cacheSolve(b_cache)
# print(b_inverted)