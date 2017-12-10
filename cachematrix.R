## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function stores matrices. we can set them with the set function and the
##get them with the get function.  We can use the get inverse function
## in conjunction with the CacheSolve function when we set the inverse.


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse <- function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function
#This function takes the inverse of the matrix that was saved in the 
#previous function.  It saves the inverse matrix so it doesn't need to 
#calculate it each time a function calls on it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  }
