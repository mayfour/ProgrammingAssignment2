## Set matrix2 <- makeCacheMatrix(YourMatrix)
## matrix2$setinv(solve(YourMaxtrix))
## Verify with matrix2$getinv() if desire
## cacheSolve(matrix2) will return the cached matrix

## First part records the cache

makeCacheMatrix <- function(x = matrix()) {
      invmatrix <- NULL
      set <- function(y) {
            x <<- y
            invmatrix <<- NULL
      }
      get <- function() x
      setinv <- function(solve) invmatrix <<- solve #Funcion for inversing is solve()
      getinv <- function() invmatrix
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Second part gets the cache

cacheSolve <- function(x, ...) {
      invmatrix <- x$getinv()
      if(!is.null(invmatrix)) {
            message("getting cached data")
            return(invmatrix)
      }
      data <- x$get()
      invmatrix <- solve(data, ...)
      x$setinv(invmatrix)
      invmatrix
      ## Return a matrix that is the inverse of 'x'
}
