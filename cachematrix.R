## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix contains the utilities to set a matrix value,fetch it
## cache inverted matrix and fetch it

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL        ##inverse to be returned
 set <- function(y)     ## set the value of matrix
 {
   x <<- y                   ##scoping to preserve environment
   inverse <<- NULL
 }
 
 get <- function() x   ##return the matrix
 
 setinverse <- function(inverse) i <<- inverse   ## cache the inverse
 getinverse <- function() i ## fetch inverse of matrix if in cache
 list(set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## return the inverse of matrix either from cache or using solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()    ## fetch inverse if it exits in cache
  if(!is.null(i))
  {
    message("getting cached data")
    return (i)
  }
  
  data <- x$get()
  i <- solve(data, ...)     ##calculate the inverse
  x$setinverse(i)           ##store in cache
  i
  
}

