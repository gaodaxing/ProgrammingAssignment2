## This function caches the inverse of a matrix rather than compute it repeatedly
## If the inverse has not been calculated, it should compute the inverse.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Invs<-NULL 
  Set<-function(y){x<<-y;Invs<<-NULL} 
  SetInvs<-function(z){Invs<<-z} 
  Get<-function()x
  GetInvs<-function()Invs 
  list(Set=Set,Get=Get,SetInvs=SetInvs,GetInvs=GetInvs)
  
}


##  This function computes the inverse of special "matrix". If  If the inverse 
##has already been calculated, then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  Invs<-x$GetInvs()
  if(!is.null(Invs)){
    message("Getting cached data")
    return(Invs)
  }
  data<-x$Get()
  Invs<-solve(data,...)
  x$SetInvs(Invs)
  Invs ## Return a matrix that is the inverse of 'x'
}
