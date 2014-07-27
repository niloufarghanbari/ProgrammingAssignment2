makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmat<-function(solve) m<<- solve
getmat<-function() m
list(set=set, get=get,
   setmat=setmat,
   getmat=getmat)
}

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmat()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get
    m<-solve(matrix, ...)
    x$setmat(m)
    m
}
