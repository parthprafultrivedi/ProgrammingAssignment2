## Put comments here that give an overall description of what your
## functions do

## It gets or sets values of matrix or inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  myInv <- NULL
  set <- function(y) {
    x <<- y
    myInv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) myInv <<- inverse
  getinverse <- function() myInv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns inverse if not computed and stores in cache, next request, 
## first checks in cache to avoid complex recomputations

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  myInv <- x$getinverse()
  if(!is.null(myInv)) {
    message("getting cached data.")
    return(myInv)
  }
  data <- x$get()
  myInv <- solve(data)
  x$setinverse(myInv)
  myInv
}
