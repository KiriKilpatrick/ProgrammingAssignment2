## cacheMatrix.R contains two functions, 'makeCacheMatrix' 
## and 'cacheSolve', that calculates and caches the inverse 
## of a matrix to be retrieved.

## 'makeCacheMatrix' function creates a special "matrix" object that
## stores a matrix and caches its inverse.

## 'x' is an invertible matrix passed in the function argument

## Return special "matrix" object containing matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
        ##Step 1: Initialize objects
        inv <- NULL
        
        ## Step 2: Define functions for setting and getting objects
        ## Set values of 'x' and 'inv' in parent environment
        set <- function(y){
             x <<- y
             inv <<- NULL
        }
        ## Get x from parent environment
        get <- function() x
        
        ## Define setter and getter for inverse
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
        ## Step 3: Create new object that returns functions as a list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## 'cacheSolve' function computes the inverse of the special "matrix" 
## returned by 'makeCacheMatrix' above. If the inverse has already 
## been calculated (and the matrix has not changed), then 'cacheSolve' 
## retrieves the inverse from the cache

## 'x' is the special "matrix" returned by 'makeCacheMatrix' above

## Return a matrix that is the inverse of 'x' or retrieve inverse
## of 'x' if cached.

cacheSolve <- function(x, ...) {
        ## Step 1: Retrieve inverse from 'makeCacheMatrix'
        inv <- x$getinv()
        
        ## Step 2: Test if inverse was retrieved
        ## If the inverse is retrieved (not null), cached inverse
        ## is returned to the console
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
       
        ## If the inverse is not retrieved (null), then
        ## Retrieve matrix 'x' from 'makeCacheMatrix',
        data <- x$get()
        ## Calculate inverse using solve function,
        inv <- solve(data, ...)
        ## Set the inverse in 'makeCacheMatrix' (caches), 
        x$setinv(inv)
        ## and returned to the console
        inv
}
