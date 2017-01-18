## The function makeCacheMatrix creates a matrix that van cache its inverse
## The function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 
## And calcultes the inverse of this matrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## initialize the inverse to null
    inverse <- NULL
    
##  Assign the input to cached matrix
##  Assign NULL to inverse
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    ## setup get, setinv and getinv to acces cache
    get <- function() x
    setsolve <- function(solve) inverse <<- solve
    getsolve <- function() inverse
    
    ## assigns each of the functions as an element within a list(), 
    ##and returns it to the parent environment.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }



## This function computes the inverse of matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  ## retrieve matrix from cache
    invmatrix <- x$getsolve()
    if(!is.null(invmatrix)) {
      message("getting cached inverse")
      return(invmatrix)
    }
    
    ## compute the inverse matrix , set cache
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    invmatrix <- solve(data)
    x$setsolve(invmatrix)
    invmatrix
}
