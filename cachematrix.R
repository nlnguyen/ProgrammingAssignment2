##
## Description: This set of (2) functions is used to calculate the inverse of
##            an invertible square matrix. The object has the ability to cache
##            the inverse matrix to save computation time.
##
## Date     : Oct. 21st, 2014
## By       : Nguyen Ngoc Lan
##

##
## makeCacheMatrix creates a special "matrix" with the ability to cache its
## inverse. The object contains a function to:
## 1. set the value of the input matrix
## 2. get the value of that matrix
## 3. set the inverse of the input matrix
## 4. get that inverse
##
## the operator <<- is used to assign a value to an object in an environ-
## ment that is different from the current environment
##
makeCacheMatrix <- function(x = matrix()) {
      matInv <- NULL
      set <- function(y){
            x <<- y
            matInv <<- NULL
      }
      get <- function() x
      
      setinverse <- function(mi) matInv <<- mi
      getinverse <- function() matInv
      
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##
## this function is used in conjunction with the function makeCacheMatrix().
## It calculates the inverse of the special "matrix" (created from the func
## makeCacheMatrix()). Its inverse is calculated only if it does not exist;
## otherwise, it is retreived from the cache
##
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      # verify and provide the cache if an inverse value exists and the existing
      # matrix has not changed (actually, if the existing matrix has changed,
      # then the inverse matrix is set to NULL. Thus, only the verification on 
      # the inverse matrix value suffice)
      inv <- x$getinverse();
      if ( !is.null(inv) ){
            message("getting cached data")
            return(inv)
      }
      
      # get the new matrix
      mat <- x$get()
      
      # ...calculate its inverse
      inv <- solve(mat)
      
      # ...cache this invese matrix
      x$setinverse(inv)
      
      # ...done!
      inv
}
