## The inverse of a matrix requires substantial calculation time.
## Therefore it is useful to store the inverse calculated for a matrix
## for later use, so as to avoid computing it all over again.

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
      # returns a list that contains the matrix and its inverse,
      # and get and set methods for both so to retrieve the information
      # or add it to the object when calculated (the inverse)
}


## cacheSolve checks whether the inverse of the matrix is available in
## in the matrix-list if not, calculates and sets it, and shows it.

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
