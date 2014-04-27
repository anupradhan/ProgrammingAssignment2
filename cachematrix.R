## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## setting matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## returing existing matrix
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    
    ## Returning cached value if inv is not null
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    ## inverting the matrix using solve() method if the matrix is not inverted before
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
