##Help functions to set, get matix, the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  	m<-NULL
 	set<-function(y){
  		x<<-y
 		m<<-NULL
  	}
  	get<-function(){ x }
	setmatrix<-function(solve) { m<<- solve }
	getmatrix<-function() { m }
	list(set=set, get=get,
   		setmatrix=setmatrix,
   		getmatrix=getmatrix)
}

##Returns cached inverse of matrix.
##If cached data not available, computes the inverse of matrix,
##cache's it, and returns computation result  

cacheSolve <- function(x=matrix()) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    datos<-x$get()
    m<-solve(datos)
    x$setmatrix(m)
    return(m)
}
