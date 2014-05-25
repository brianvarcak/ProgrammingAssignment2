## Following functions will combine to create a matrix that can store and recall the inverse of itself

## Function creates a matrix that gives a user the capability to store both the original matrix and the
## cached version of the inverse of the matrix.  Both versions can be initialized as null and set
## at a later time

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  
  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse)
  
  
}


## Function checks to see if the input CacheMatrix contains a value for the inverse of the input matrix.
## if no cache version of the inverse is found, the inverse will be calculated and added to the CacheMatrix

cacheSolve <- function(x, ...) {
  ## Check to ensure a matrix exists for the input CacheMatrix
  if(!is.null(x$get())){
    
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    if(!is.null(i)){
      message("Getting Cached Inverse")
      return(i)
    }
    
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    
    return(i)

  }
    
  message("No Matrix Provided")

}
