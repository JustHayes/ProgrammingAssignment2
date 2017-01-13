## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
## of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we
## will not discuss here). The following functions cache the invers of a matrix

## MakeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cachematrix <- NULL
  set <- function(y){
    x <<- y
    cachematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solution) cachematrix <<- solution
  getinverse <- function() cachematrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachematrix <- x$getinverse()
  if(!is.null(cachematrix)){
    message("retrieving cache")
    return(cachematrix)
  }
  
  matrixdata <- x$get()
  cachematrix <- solve(matrixdata, ...)
  x$setinverse(cachematrix)
  cachematrix
}