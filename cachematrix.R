## Put comments here that give an overall description of what your
## functions do

## This function is to create a cache matrix

makeCacheMatrix <- function(x=matrix())
{
  m <- NULL
  set <- function(y) ## set the matrix
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x ## get the matrix
  setinv <- function(inv) m <<- inv  ## set the inversed matrix
  getinv <- function() m  ##get the inversed matrix
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

  
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) ## if the inversion has already been calculated, direct read the existing data
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## use "solve" function to determine the inversion of the matrix, default S3 method.
  x$setinv(m)
  m
}

