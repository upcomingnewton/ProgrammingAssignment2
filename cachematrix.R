## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function defines various methods to calculate
## and get the inverse of a matrix
## Following functions are defined in this function
## set -  to set the matrix
## get - to get the matrix
## setinverse - to set inverse of the matrix
## getinverse - to get the inverse of the matrix
## It sets a vector(list), which contains functions to all above

makeCacheMatrix <- function(x = matrix()) {
	
   # mark a variable to contain inverse value of the matrix
   inverse_matrix <- NULL
   
   # make a function  to set value for matrix, and also NULL to its inverse
   set <- function(y) {
   	  x <<- y
   	  inverse_matrix <<- NULL
   }
   
   # return the matrix
   get <- function() x
   
   # set the computed value of inverse of matrix to inverse_matrix
   # this will be called to set inverse of matrix to cache
   setinverse <- function(inv) inverse_matrix <<- inv
   
   # get the inverse of matrix from cache
   getinverse <- function() inverse_matrix
   
   # return all the function
   list( set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
   
}


## Write a short comment describing this function

## This function calculates the inverse of the matrix, created with above function.
## if inverse if already calculated, it gets it from cache and skips the computation.
## else, it will calculate the value and set it via setinverse to cache, and then return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get the inverse of matirx, x
        x_inverse = x$getinverse()
        
        # check if inverse is not null
        if(!is.null(x_inverse)) {
        	message("getting cached data")
        	
        	# if inverse is not null, then return it
			return(x_inverse)
        }
        
        # inverse of x is null, hence get the matix
        matrix <- x$get()
        
        # calculate it's inverse
        x_inverse <- solve(matrix)
        
        # set the inverse to cache
        x$setinverse(x_inverse)
        
        # return inverse
        x_inverse
}


# source("cachematrix.R")
# m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
# t <- makeCacheMatrix(m)
# cacheSolve(t)

##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

# cacheSolve(t)

##	getting cached data
##	     [,1] [,2]
##	[1,]   -2  1.5
##	[2,]    1 -0.5