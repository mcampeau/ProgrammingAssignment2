## These functions are used to create a cache version of a matrix and its inverse

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
      # initialize
	  mInverse <- NULL
	  # cache data
      set <- function(y) {
            x <<- y
            mInverse <<- NULL
      }
      get <- function() x
	  # inverse is a inverted matrix
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
        ## (and the matrix has not changed), then the cacheSolve 
        ## retrieves the inverse from the cache      
      
      mInverse <- x$getInv()
	  # verify if it is cached
      if(!is.null(mInverse)) {
            message("getting cached data")
            return(mInverse)
      }
	  # otherwise calculate inverse and cache it, before returning it
      data <- x$get()
      mInverse <- solve(data, ...)
      x$setInv(mInverse)
      mInverse      
      
}
