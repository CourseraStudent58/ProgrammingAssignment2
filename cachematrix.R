## cacheMatrix
## wrapper for a matrix class which caches the inverse of the matrix
## so repeated references to the inverse will have better performance.
##
## Usage: makeCacheMatrix() to define the matrix
##        solveCacheMatrix() to retrieve the inverse

## makeCacheMatrix()
## Input: a matrix
## Return: a vector of functions to operate on cacheMatrix
## Description: Constructor function for cacheMatrix class.  
## Defines a set of functions for get/set of matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # private vars of class
    cache.val <- NULL
    
    #define public methods of class
    set <- function(y) {
        x <<- y
        cache.val <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) cache.val <<- inverse
    
    getinverse <- function() cache.val
    
    # return list of public members
    list( set=set, get=get,
          setinverse = setinverse,
          getinverse = getinverse )
}


## cacheSolve()
## Input: a cacheMatrix, and optional arguments to solve()
## Return: a matrix that is the inverse of the matrix used to instantiate the class
## Description: uses a cached value for the inverse if it has been previously calculated.
## Otherwise, it calculates the inverse and places it in the cache
cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()   # attempt to get from cache
    if( !is.null(m)) { 
        message( "getting cached data")
        return(m)
    }
    data <- x$get()        # retrieve input matrix
    m <- solve(data,...)   # calculate inverse, passing any extra solve args
    x$setinverse(m)        # set cache
    
    m  #return inverse
}

## cacheSolveTest()
## Input: none
## Return: none
## Description: creates a few sample cacheMatrix of random values, and prints the cacheSolve values to
## demonstrate a) after the first solve, the value comes from the cache
##             b) the value returned matches what solve would return for the same input
cacheSolveTest <- function() {
    mat <- matrix( sample(1:10,4,replace=T), 2, 2 )
    cm <- makeCacheMatrix( mat )
    print ("test matrix:" )
    print( mat )
    print ("cacheSolve result:" )
    print( "first solve")
    print( cacheSolve(cm) )
    print( "second solve")
    print( cacheSolve(cm) )
    print( "uncached solve result")
    print( solve(mat) )
    
    mat <- matrix( sample(1:10, 9, replace=T), 3, 3 )
    cm <- makeCacheMatrix( mat )
    print ("test matrix:" )
    print( mat )
    print ("cacheSolve result:" )
    print( "first solve")
    print( cacheSolve(cm) )
    print( "second solve")
    print( cacheSolve(cm) )
    print( "uncached solve result")
    print( solve(mat) )
}
