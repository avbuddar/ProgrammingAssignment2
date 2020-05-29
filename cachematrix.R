#Week 3 Programming Assignment

#***** Sample Script provided by Instructor *****
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }

# makeCacheMatrix Function
# This function creates matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL #Returns NULL if the Matrix is NOT cached
  
  set <- function(y){
    x <<- y
    inv <<- NULL #Assigning value to object(inv) in the Parent Environment
  }
  
  get <- function(){
    x
  }
  
  setInverse <- function(inverse){
    inv <<- inverse #Assigning value to object(inv) in the Parent Environment
  }
  
  getInverse <- function(){
    inv
  }
  
  #Set and Get values to the Matrix
  #Set and Get Inverse values to the Matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#***** Sample Script provided by Instructor *****
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }

# cacheSolve Function 
# This function computes the inverse of the matrix returned by 
# makeCacheMatrix function. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
 
   if(!is.null(inv)){
    message("getting cached data") 
     #Inverse already calulated. 
     #Retreiving from cache
    return(inv)
   }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv # Returns a matrix that is the inverse of Matrix x
}


