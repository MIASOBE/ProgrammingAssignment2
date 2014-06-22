## The following two functions were implemented to support the calculation of the inverse of an invertible
## matrix and the ability to cache the result. This can be used to improve performance if this process
## needs to be repeated many times.

## This function creates a special "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
          ## Initialize m - used to cache the inverse of the matrix
          m <- NULL
          
          ## Define and implement set, get, setInverse,and getInverse functions
          set <- function(y) {
              x <<- y
              ## When a new matrix is set, need to make sure to clear the cached one
              m <<- NULL
          }
          get <- function() x
          
          ## Used to set the cached inverse of the matrix
          setInverse <- function(inverse) m <<- inverse
          
          ## Used to get/return the cached inverse of the matrix
          getInverse <- function() m
          
          ## Return a list of the above functions
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
  
      
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Tries to get the inverse of the matrix in the list passed in
        m <- x$getInverse()
        
        ## If it is not null then just use the cached version
        if(!is.null(m)) {
              ## This message just lets the user know that a cached version of the inverse is being returned,
              ## but this can be removed if we don't want to get this message
              message("Returning Cached Version")
              
              ## Return the cached version of the inverse
              return(m)
        }
        
        ## If nothing is returned then need to get the matrix from the list passed in
        data <- x$get()
        
        ## and compute the inverse of the matrix returned
        m <- solve(data, ...)
        
        ## Set the cached version in x
        x$setInverse(m)
        
        ## Return the inverse of the matrix
        m
  
}
