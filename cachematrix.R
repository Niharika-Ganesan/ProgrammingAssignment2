## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## makeCacheMatrix and cacheSolve functions act together to cache and deliver the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse,  which is really a list of functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inversed matrix value 
## 4. get the inversed matrix value 

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
