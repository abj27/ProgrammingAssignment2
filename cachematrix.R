## 
## Here we have 2 methods the first one is a factory that generates a cacheable-matrix using a matrix as a parameter
## The second one takes a cacheable-matrix object and computes its inverse.


## cacheMatrix makeCacheMatrix(matrix)
## void set(matrix):
## All the cached variables are reseted (set to NUll) and the internal matrix is set to the new one
## matix get():
## returns the internal matrix
## void setinverse(numeric): 
## set the cachedInverse to the parameter
## numeric getinverse():
## returns the current cached inverse value

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  
  set <- function(matrix){
    x <<- matrix
    cachedinverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) cachedinverse <<- inverse
  
  getinverse <- function() cachedinverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}


## numeric cacheSolve(cacheMatrix)
## Calculates the inverse of a cacheMarix
## If the cacheMatix has an inverse cached value set up (that is not null)
## it will return the cached value, otherwise it will compute it using the solve function
## and then set the cached inverse value to the cacheMatrix for future usages.
##

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
      return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
