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
  m <- solve(matrixdata, ...)
  x$setinverse(cachematrix)
  cachematrix
}

##  makeVector creates a special "vector", which is really a list containing a function to
##    1) set the value of the vector
##    2) get the value of the vector
##    3) set the value of the mean
##    4) get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## The following function calculates the mean of the special "vector" created with the above function.
## However, it first checks to see if the mean has already been calculated. If so, it gets the mean
## from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the
## value of the mean in the cache via the setmean function.
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}