## This function calculates inverse of matrix and returns data from cache.
## We will break the problem in two functions.
##makeCacheMatrix is a constructor that will get and set values for original and inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  invx<-NULL
  set<-function(y){
    x<<-y
    invx<-NULL
  }
  get<-function(){
    return(x)
  }
  setInv<-function(y){
    invx<<-y
  }
  getInv<-function(){
    invx
  }
  list(set=set, get = get, setInv=setInv, getInv=getInv)
    
}


## cacheSolve will take the matrix and  calculate its inverse if not already calculated
## if inverse is already store in cache it will return the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx<-x$getInv()
  if(!is.null(invx))
  {
    print("returning cached value")
    return(invx)
  }
  valuex<-x$get()
  inv<-solve(valuex)
  x$setInv(inv)
  inv
}
