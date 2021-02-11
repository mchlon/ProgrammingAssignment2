## Put comments here that give an overall description of what your
## functions do

##This function creates "vector"
##which is a list of functions which sets the value the vector, gets the value
##sets the value of the mean and gets the value of the mean.
makeCacheMatrix <- function(x = matrix()) {
      m<- NULL
      set<- function(y){
              x<<-y
              m<<-NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set=set, get=get,
           setinverse=setinverse,
           getinverse=getinverse)
      
}


## This fn calculates the inverse of the matrix, but checks to see
## if this hasnt been done before, if it has it grabs the value of the 
##inverse from the cache. 

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
