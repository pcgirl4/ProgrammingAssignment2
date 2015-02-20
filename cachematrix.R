## This pair of function can cache the inverse of a matrix to save time doing 
## this operation. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinverso <- function(inverso) inve <<- inverso
  getinverso <- function() inve
  list(set = set, get = get,
       setinverso = setinverso,
       getinverso = getinverso)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inve <- x$getinverso()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setinverso(inve)
  inve
}
