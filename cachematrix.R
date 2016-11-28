## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache 
## its inverse.  
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <-NULL   
  ## assigning m to NULL so that it changes each time the function 
  ## is called 
  set <- function(y) {
    x <<- y 
    m <<- NULL  
    ##keeping the value m as null so that it resets each time the 
    ##function is run 
  }
  get <- function() x
    ##get the value of the matrix - x outside the parenthesis 
    ##says get x 
  setinverse <- function(solve) m <<- solve
    ## set the function to get the inverse of the matrix 
  getinverse <- function() m
    ## get the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## set the value of "m" to get the inverse of the matrix "x"
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ##get the value of the matrix x
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ##cachesolve function looks to seee if the inverse has been calculated
  ##if so, provides the value of the inverse and if not, calculates
  ##the value of the inverse. The solve function only calculates the 
  ##inverse of the matrix. 
}
