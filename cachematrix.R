## Two functions 
## Function one is makeCacheMatrix:
##Create a special matrix and do the following
##  set the values of the matrix
##  get the values of the matrix
##  set the values of the inverse
##  get the values of the inverse

## Functions two is cacheSolve:

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
          x <<- y
	  i <<- NULL
     }
     get <- function() x
     setinverse <- function (inverse)  i <<- inverse
     getinverse <- function() i
     list( set = set, get = get, 
          setinverse = setinverse, getinverse=getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, makeCacheMatrix(x,)) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)) {
          message("getting cached data")
	  return(i)
      }	  
     data <- x$get()
     i <- solve(data)
     x$setinverse(i)
     i

}
