##Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than computing it repeatedly.

## makeCacheMatrix creates a special 'matrix' that contains a list of functions that:
## a) Set the value of the matrix
## b) Get the value of the matrix
## c) Set the value of the inverse
## d) Get the value of the inverse
## Where m is the matrix and i is the inverse of the matrix. 
makeCacheMatrix <- function(m = matrix()) {
  ##Start invers, i value of NULL
  i <- NULL
  ## set will assign the matrix and reset the inverse to NULL
  set <- function(x) {
    m <<- x
    i <<- NULL
  }
  ## get returns the current matrix
  get <- function() m
  ## setinverse will assign the inverse value
  setinverse <- function(inverse) i <<- inverse
  ## getinverse will return the inverse value
  getinverse <- function() i
  ## create the list vector for the functions.
  list(set = set, get = get, 
       setInverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix if it exists.
## Calculates and returns the inverse of a matrix if it does not exist.

cacheSolve <- function(x) {
  ##Check if inverse is available
  inverse <- m$getinverse()
  ##If the value is not NULL then return the value of inverse.
  if(!is.null(i)) {
    message('getting cached data')
    return(i)
  }
  ##Otherwise, get data of the matrix.
  Gmatrix <- m$get()
  ##Solve Matrix, m 
  i <- solve(Gmatrix)
  ##And cache in inverse.
  x$setinverse(i)
  ##Return Value of inverse.
  return(i)
}
