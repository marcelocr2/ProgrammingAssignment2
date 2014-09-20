## these are the function for the assignment. It is important to run makeCacheMatrix first

## This function recives a matrix and has for function in it. get, set, getinv and set inv. getinv returns the matrix
## inv, setinv() set the value of the matrix inv

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function()
  {
    x
  }
  setinv<-function(inver)
  {
    inv<<-inver
  }
  getinv<-function()
  {
    inv
  }
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## this function calls getinv() and sees if inv has a value different than null. If it is true returns cached matrix inv
## else calculates the inverse of the matrix and saves it in inv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv=x$getinv()
  if(!is.null(inv)) {
    message("Getting cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
