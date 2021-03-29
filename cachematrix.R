## The functions support the calculation and caching of the inverse of a matrix.
## If the inverse of a specific matrix has already been calculated, 
## the cached inverse matrix will be returned, thus significantly cutting down
## on processing time.

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse to facilitate the caching of inverted matrices.
## The following functions are returned to be used for the matrix object:
## creates a special "matrix" object that can cache its inverse.
## cache the value of the original matrix (set)
## get the value of the original matrix (get)
## add the value of the inverted matrix to the cache (setinv)
## retrieve the value of the inverted matrix from the cache (getinv)

makeCacheMatrix <- function(x = matrix()) {
    matr <- NULL
    set <- function(y) {
        x <<- y
        matr <<- NULL
    }
    get <- function() x
    setinv <- function(solve) matr <<- solve
    getinv <- function() matr
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## The cacheSolve function takes as an input a cache matrix object and returns 
## the inverse of the matrix of the object.
## The function first checks if the inverse is already cached and returns the cached 
## matrix if that is the case.
## If not yet cached, the inverse is calculated, cached and returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matr <- x$getinv()
    if(!is.null(matr)) {
        message("getting cached data")
        return(matr)
    }
    ## Calculate inverse, cache and return
    data <- x$get()
    matr <- solve(data, ...)
    x$setinv(matr)
    matr
}
