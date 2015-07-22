## This program contains 2 functions. The first function is to cache the inverse of the input matrix.
## The second function will compute the inverse of the input matrix and store it, if the inverse 
## has been calculated, it will retrieve the inverse from the cache.

## This function creates a list of 4 matrices. 
## Matrix 1 is to set the input matrix.
## Matrix 2 is to get the input matrix.
## Matrix 3 is to set the inverse of input matrix.
## Matrix 4 is to get the inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function computes the inverse of the matrix in makeCacheMatrix$get(), 
## then store it into getinv(). If the input matrix does not change,
## it will return the cache inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  mat <- x$get()
  m <- solve(mat, ...)
  
  x$setinv(m)
  m## Return a matrix that is the inverse of 'x'
}
