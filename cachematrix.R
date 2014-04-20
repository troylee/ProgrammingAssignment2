## This function creates a special square matrix which is similar to the basic 
## square matrix but with the additional caching capability of its inverse.
## This would be helpful when frequent access of the matrix and its
## inverse is required, especially for those with large sizes.

## This function creats a specal square matrix object that can cache its inverse. 
## It is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialized the inverse matrix to be NULL
  inv <- NULL
  ## function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## function to get the value of the matrix
  get <- function() x
  ## function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  ## function to get the inverse of the matrix
  getInverse <- function() inv
  ## return the specal matrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function caculates the inverse of the specal matrix created above.
## It first checks whether the inverse has already been calculated. If so, 
## it returns the cached value; otherwise, it calcuates the inverse of the 
## data and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## first retrieve the cached inverse matrix
  inv <- x$getInverse()
  ## return the inverse matrix if it is not NULL, i.e. the inverse has been
  ## calcuated previously
  if(!is.null(inv)) {
    ## print out some debuging message
    message("getting cached inverse")
    ## return the value
    return(inv)
  }
  ## get the matrix itself 
  data <- x$get()
  ## compute its inverse
  inv <- solve(data, ...)
  ## cache the computed inverse
  x$setInverse(inv)
  ## return the inverse of 'x'
  inv
}
