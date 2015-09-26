## The two functions below creates an object for storing matrix
## and it's inverse, together with cacheSolve which actually do
## the computation to calculate the inverse matrix.
## Example usage:
##
##   # Create an object to store the matrix
##   m = makeCacheMatrix( matrix(1:4, nrow =2, ncol = 2) )
##
##   # Calculate the inverse of the matrix,
##   m1 = cacheSolve(m)
##
##   # Calculating the inverse matrix again will be from the cached results
##   # hence faster performance.
##   m2 = cacheSolve(m)

## makeCacheMatrix -- creates an object to hold a matrix and a cache
## for its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(mtrx) m <<- mtrx
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve -- Perform matrix inverse computation, takes matrix object
## created from makeCacheMatrix function. If previous computation was done
## the cached value is return. Print message to indicate whether the result
## is from cached value or new computation.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    message("solving matrix")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m    
}

## unitTest -- unit test code for makeCacheMatrix() and cacheSolve()
unitTest <- function() {
    # init new matrix
    x <- matrix(1:4, nrow =2, ncol = 2)
    y <- matrix(11:14, nrow =2, ncol = 2)
    
    # init cache and non cache data
    data1 <- makeCacheMatrix(x)
    data2 <- makeCacheMatrix(y)
    
    # test get and set matrix for data1
    data1$set(y)
    data1$get()
    
    # it should return from computation
    m1 = cacheSolve(data1)
    
    # it should return from cache
    m2 = cacheSolve(data1)
    
    # it should return from computation
    m3 = cacheSolve(data2)        
}


