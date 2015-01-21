## These functions are used to crete a cache version of a matrix aits inverse

## This function, makeCacheMatrix, creates a special "matrix" object, 
## mInverse, that can cache its inverse
## 
## X is the matrix that we want to cache its inverse
## it returns a list containing a function to
## set the value of the matrix, set()
## get the value of the matrix, get()
## set the value of the inverse of the matrix, setInv()
## get the value of the inverse of the matrix, getInv()

makeCacheMatrix <- function(x = matrix()) {
      mInverse <- NULL
      set <- function(y) {
            x <<- y
            mInverse <<- NULL
      }
      get <- function() x
      setInv <- function(inverse) mInverse <<- inverse
      getInv <- function() mInverse
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)      

}


## CacheSolve return a matrix that is the inverse of 'x',
## a matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## This function gets or computes the inverse of 
        ## the special matrix object, "mInverse" 
        ## created by makeCacheMatrix function
        ## If the inverse has already been calculated 
        ## (and the matrix has not changed), then the cachesolve 
        ## retrieves the inverse from the cache      
      
      mInverse <- x$getInv()
      if(!is.null(mInverse)) {
            message("getting cached data")
            return(mInverse)
      }
      data <- x$get()
      mInverse <- solve(data, ...)
      x$setInv(mInverse)
      mInverse      
      
}
