## I have written 2 functions. Function makeCacheMatrix creates a matrix object 
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y)
  {
    x<<-y
    inverse<<-NULL
  }
  get<-function () x
  setinverse<-function(y)
  {
    inverse<<-y
  }
  getinverse<-function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function cacheSolve computes the inverse. it it is not already computed
## in case the inverse is computed already, it returns the cached value of inverse.

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!(is.null(inverse)))
  {
    message("getting Cached data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
}
