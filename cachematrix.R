## A pair of functions to get the inverse of a matrix. If it the inverse been computed previously the functions will 
##get the inverse from a cache, otherwise the inverse will be computed


## makeCacheMatrix creates a special "matrix" object
##this is a list containing a function to; set the values of the matrix,
##get the values of the matrix, set the inverse of the matrix, and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
        x <<- y
        i <<- NULL
  
}
  get <- function() x
  setinverse<-function(inverse) i <<- inverse
  getinverse <-function() i
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
  
}


## cacheSolve calculates the inverse of the special matrix if it has not been calculated already
## otherwise cacheSolve retrieves the inverse from a cache.

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message ("getting cached data")
    return(i)
  }
  
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
  
}

#test the functions
a<-matrix(data = c(1,2,1,2,3,3,2,2,1), nrow = 3)
pp<-makeCacheMatrix(a)
cacheSolve(pp)


