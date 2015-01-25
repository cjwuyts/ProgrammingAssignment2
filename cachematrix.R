## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix makes a list containing get and set methods for both
## the matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set<-function(y){
            x<<-y
            inv<<-NULL
      }
      get<-function() x
      setinv<-function(inverse) inv <<- inverse
      getinv<-function() inv
      list(set=set, get=get,setinv=setinv,
           getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inv<-x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data<-x$get()
      inv<-solve(x$get)
      x$setinv(inv)
      inv
        ## Return a matrix that is the inverse of 'x'
}


cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}