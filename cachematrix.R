# These are two functions that are used to create a special object
# that stores a matrix and caches its inverse

# makeCacheMatrix creats a list containing functions to
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse
# 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){ #function to set x
    x<<-y
    m<<-NULL
  }
  get<-function() x #function to get x
  setsolve<-function(solve) m<<- solve #function to solve x
  getsolve<-function() m  #function to get the inverse matrix m
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

# The following function calculates the inverse of the special matrix
# created with the above function. However it checks to see if the 
# inverse has already been calculated. If so it gets the inverse from
# the cache. Otherwise, it calculates the inverse and sets the value
# of the inverse in the cache via the setmean function.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m) #return the already calculated inverse
  }
  data<-x$get()
  m<-solve(data, ...) #calculate the inverse
  x$setsolve(m) #set the value of the inverse in the cache
  m #print the inverse
}