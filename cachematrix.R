## Two functions 
## Function one is makeCacheMatrix:
## Function two is cacheSolve:


##  makeCacheMatrix Function
##  Create a special matrix and put the following functions in a list
##  set the values of the matrix
##  get the values of the matrix
##  set the values of the inverse
##  get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
          x <<- y
	    i <<- NULL         ## set i to NULL first time
     }
     get <- function() x
     setinverse <- function (inverse)  i <<- inverse
     getinverse <- function() i
     list( set = set, get = get, 
          setinverse = setinverse,
          getinverse=getinverse)
}


## cacheSolve function
## compute the inverse of a matrix - assume it is invertible
## if already computed return the previously computed value cached in MakeCacheMatrix

cacheSolve <- function(x,...) {      ## x should be matrix created by makeCacheMatrix
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)) {
          message("Getting cached data -not solving again")
	  return(i)
      }	  
     data <- x$get()      ## get MakeCacheMatrix from list of functions
     i <- solve(data)     ## find inverse
     x$setinverse(i)      ## call setinverse function in MakeCacheMatrixList
     i
}

## test case
a <- matrix(1:4,2,2) #create 2 x 2 matrix
print ("test matrix")
print (a)
ans <- solve(a)      #generate inverse to compare to function
print ("direct solve of inverse of test matrix for comparison")
print (ans)
b <- makeCacheMatrix(a) #assign intial make cache to b
cacheSolve(b)        # generate inverse first time

b$get() #get original matrix
b$getinverse() #get inverse
print (ans)    #print test inverse - should be the same

cacheSolve(b)  #this time should return cached value rather than computing 
