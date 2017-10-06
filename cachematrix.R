## Created By: Jeevan Mahajan
## Created On: 6-Oct-2017
## Coursera Data Science Certification, Course 2, Week 3, Programming Assignment

## This programming assignment is to write an R function that is able to cache potentially time-consuming computations.
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. This assignment is to write a pair of functions that cache the inverse of a matrix.
## For this assignment, it is assumed that the matrix supplied is always invertible

## makeCacheMatrix - This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initiaze variable
  specialm <- NULL
  
  set <- function(y) {
    x <<- y
    specialm <<- NULL
  }
  
  ## Return the input matrix.
  get <- function() x
  
  ## Store the inverse of the Matrix in Cache for future use.
  setinverse <- function(inversematrix) specialm <<- inversematrix
  
  ## Get the currently inversed matrix details.
  getinverse <- function() specialm
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##              If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves
##              the inverse from the cache.
##              Computing the inverse of a square matrix can be done with the solve function in R. 
##              For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
  
  ## Retrieve the inverse matrix values.
  specialm <- x$getinverse()
  
  ## If the inverse is already calculated the returned the cached value.
  if(!is.null(specialm)) {
    message("Getting cached data")
    return(specialm)
  }
  
  ## If inverse is not calculated then get the input matrix.
  matrixdata <- x$get()
  
  ## Calculate the inverse of the input matrix using solve() function.
  specialm <- solve(matrixdata, ...)
  
  ## Cache the newly calculated inverse matrix.
  x$setinverse(specialm)
  
  ## Return inverse matrix
  specialm
  
}
