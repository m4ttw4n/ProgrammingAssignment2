## Coursera - R Programming - Programming Assignment #2
## Student: Matt Wan
## Description: 
## Calculating the inverse of a matrix can be resource intensive.
## The following two functions allow for the caching of a matrix inverse to
## provide potential computational resource savings for future calculations.

############################
## Function: makeCacheMatrix
## This function creates a list of functions to do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # Clears the object i, that is, the inverse of the input matrix
        i <- NULL 
        
        # 1. set the value of the matrix
        set <- function(y) { 
                x <<- y
                i <<- NULL
        } 
        
        # 2. get the value of the matrix
        get <- function() x 
        
        # 3. set the value of the matrix inverse
        setinverse <- function(inverse) i <<- inverse
        
        # 4. get the value of the matrix inverse
        getinverse <- function() i
        
        # Construct the list of functions
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        

}

############################
## Function: cacheSolve
## This function returns the inverse of the matrix inputted into and created
## by the makeCacheMatrix function. The function will aim to save computation
## resources by calculating the inverse only if the inverse has not already been
## calculated and cached previously.

cacheSolve <- function(x, ...) {
        
        # Call up the i variable from makeCacheMatrix
        i <- x$getinverse()
        
        # Checks to see if the inverse was calculated and cached previously.
        # If so, returns the cached inverse.
        if(!is.null(i)) {
                message("retrieving cached data")
                return(i)
        }
        
        # When a cached inverse does not exist, the inverse is calculated,
        # cached, and returned.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}