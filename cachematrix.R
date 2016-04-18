# A pair of functions that creates the inverse of a matrix

## makeCacheMatrix  is a function which is used to get the inputted matrix and also gets/sets the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL 
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  
  get <- function() x 
  setInv <- function(inv) xinv <<- inv 
  getInv <- function() xinv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve is a function to return a matrix this is inverse of x by trying to get inverse of matrix initially and if it is null
# then gets the matrix in data and computes the inverse of matrix and sets it to m and return inversed matrix m

cacheSolve <- function(x, ...) {
          m <- x$getInv() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) 
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m 
}
