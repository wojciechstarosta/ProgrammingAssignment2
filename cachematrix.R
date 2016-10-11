makeCacheMatrix <- function(x = matrix()) {
  ## x - a square invertible matrix
  
  inv = NULL
  set = function(y) { ##set the matrix
    x <<- y ## assigning a value to an environment different than current one
    inv <<- NULL
  }
  get = function() x ##get the matrix
  stored <<- x ##store the value of matrix so check for equality can be made
  setinv = function(inverse) inv <<- inverse ##set the inverse
  getinv = function() inv ##get the inverse
  getStored <- function() stored ##get stored
  list(set=set, get=get, setinv=setinv, getinv=getinv, getStored = getStored)
}

cacheSolve <- function(x, ...) {
  ## x - output of makeCacheMatrix()
  
  inv = x$getinv() ##gets inverse if it was calculated
  
  if (!is.null(inv)){ ##check if inverse was calculated
    if(identical(x$getStored(),x$get())) { ##check if new matrix is the same as matrix in cache
    message("getting data from cache")
    return(inv) ##returning matrix from cache
    }
  }
  
  mat.data = x$get() ##assign matrix
  inv = solve(mat.data, ...) ##calculating inverse
  x$setinv(inv) ## sets the value of the inverse in the cache by the setinv function.
  
  return(inv) ##returning inverse matrix
}