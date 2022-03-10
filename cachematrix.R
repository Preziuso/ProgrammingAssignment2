
library(matlib)
makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x)==nrow(x) && det(x)!=0) {
    m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setinverse <- function() m <<- inv(x)
    getinverse<-function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
  }else{
    return(message("The matrix is'n invertible."))
  }
}
cacheSolve <- function(x, ...) {
  m<-x$getinverse
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get
  m <- inv(data, ...)
  x$setinverse(m)
  m
}
x<-makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3))
x$get()
x$getinverse()