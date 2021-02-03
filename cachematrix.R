## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores in cache the inverse of a matrix to be used for future
## It takes as input a matrix x
## the output is a list

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function()x
      setInverse <- function(inverse) m <<- inverse
      getInverse <- function() m 
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## This function returns the inverse of the matrix "x"
        ## x is the result of the makeCacheMatrix function on x
        ## It checks if there is an already computed inverse
        ## If that doesn't exist will it compute it
      m <- x$getInverse()
      if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
            }
            matr <-  x$get()
            m <- solve(matr, ...)
            x$setInverse(m)
            m
}
