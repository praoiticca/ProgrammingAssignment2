## write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should
##retrieve the inverse from the cache.

## This function creates a special "matrix", which is really a list of
## functions.
## 1 set the value of the matrix, ans reset to NULL the inv value.
## 2 assign or caches the matrix in the function getmatrix().
## 3 set the inverse matrix obtained to inv in contained in a parent
## enviroment.
## 4 cache the inv matrix in the function getinverse().


makeCacheMatrix <- function(x = matrix()) {
     inv<-NULL
     setmatrix<-function(y){
          x<<-y
          inv<<-NULL
     }
     getmatrix<-function() x
     setinverse<-function(inverse) inv<<-inverse
     getinverse<-function() inv
     list(setmatrix=setmatrix, getmatrix=getmatrix,
          setinverse=setinverse, getinverse=getinverse)
}


## This function takes the inv matrix cached in the special "matrix" 
## list of functions and assigns it to local inv variable.
## If the the local inv is not a NULL, means that the inverse matrix is 
## already calculated. So return(inv) stops the whole function and 
## assign the inv value to te function cachesolve().
## But if inv = NULL, Calculate the inverse matrix of the special "matrix"
## stored in makeCachematrix() and set the inverse of this matrix with
## setmean(inv) function defined in makeCachematrix(). 

cacheSolve <- function(x=makeCacheMatrix(), ...) {
        
     inv<-x$getinverse()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv) ## Return a matrix that is the inverse of 'x'
     }
     data<-x$getmatrix()
     inv<-solve(data,...)
     x$setinverse(inv)
     inv
}
