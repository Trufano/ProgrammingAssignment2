#These functions make a special kind of matrix that can cache its own inverse matrix

#The function makeCacheMatrix creates a special kind of matrix that caches its inverse. It can
#set the matrix, get it, set the inverse, and get the inverse
makeCacheMatrix<-function(X=matrix()){
  inv<-NULL
  set<-function(Y){
    x<<-Y
    inv<<-NULL
  }
  get<-function()X
  setinverse<-function(I) inv<<-I
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
#The cacheSolve function gives back a matrix´s inverse. It finds out if the inverse matrix has already 
#been calculated, and if so it extracts it from the special matrix an returns it, if not it calculates
#the inverse and saves it with the original matrix so it can be extracted later.
cacheSolve<-function(X,...){
  inv<-X$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data<-X$get()
  inv<-solve(data,...)
  X$setinverse(inv)
  inv
}
