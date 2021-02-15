## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL             #assign inverse to NULL
  set <- function(y){
              x <<-y
              inv <<- NULL
}
  get <- function() {x}
  setInverse <- function(inverse){inv <<- inverse}#function to obtain inverse function
  getInverse <- function() {inv}
  list( set= set, get= get,setInverse = setInverse,getInverse= getInverse)
}


cacheSolve <- function(x, ...) {               #get cache data
 
  if( is.null(inv)){                            #checking whether inverse in NULL
    message("getting cached data")               
    return(inv)                                 #return inverse value
  }
  mat <- x$get()
  inv <- Solve(mat,....)
  x$setinverse(inv)
  inv
}
