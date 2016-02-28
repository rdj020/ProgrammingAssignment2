## "Caching the Inverse of a matrix" 
## RDJ 2016-02-28 for Programming Assignment 2
## Week 3 of R Programming - Data Science Specialization
##
## This script defines a pair of functions that cache the inverse of a matrix 
## in a special "matrix".
## 
## Script checks for change in matrix and cached inverse.
## If matrix is changed it will cache new matrix and it's inverse.
## If matrix is not changed and inverse is not cached it will
## create and cache inverse.
## If matrix is not changed and inverse is cached it will get inverse
## data from cache instead of computing it.
##  
## makeCacheMatrix() creates special matrix object to store matrix and it's 
## inverse in cache.
## It defines 4 functions in a list: set(), get(), setinverse() and getinverse()
## for handling the cache.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve() checks for inverse cached and possible change in matrix.
## If so use cached data, if not compute inverse and cache.
## To check whether cached matrix is same as current matrix it takes these
## arguments:   x is the list created by makeCacheMatrix 
##              y is current matrix

cacheSolve <- function(x, y, ...) {
        
        if(identical(x$get(), y)) {     # check for change in matrix
                i <- x$getinverse()     
                if(!is.null(i)) {       # check for inverse in cache
                        message("Getting cached data")
                        return(i)        
                }
                data <- x$get()         # create inverse from cached matrix
                i <- solve(data)        
                x$setinverse(i)
                i
        }
        else {                          # matrix changed
                x$set(y)                # cache matrix
                i <- solve(y)
                x$setinverse(i)         # cache inverse
                i
        }
}

## Example

m <- matrix(c(1.2, 1.4, 1.2, 3.1), 2, 2) 
n <- matrix(c(1.3, 1.4, 1.2, 3.2), 2, 2)        # changed one digit in first item 
mm <- makeCacheMatrix(m)                        # creates special "matrix" from m and
                                                # stores matix m in cache
mm$get()                                        # gives matrix m as output
cacheSolve(mm, m)                               # creates and caches inverse from m
cacheSolve(mm, n)                               # caches matrix n and creates and caches
                                                # inverse from n
mm$get()                                        # gives newly cached matrix n as ouput

