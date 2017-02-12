## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
      nr = nrow(x)
      nc = ncol(x)
      inverx <- matrix(nrow = nr, ncol = nc)
      set <- function(y) {
            x <<- y
            nr = nrow(x)
            nc = ncol(x)
            inverx <- matrix(nrow = nr, ncol = nc)
      }
      get <- function() x
      setinverx <- function(inversa) inverx <<- inversa
      getinverx <- function() inverx
      list(set = set, get = get,
           setinverx = setinverx,
           getinverx = getinverx)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
              inverx <- x$getinverx()
      if(!is.na(inverx[1,1])) {
            message("getting cached data")
            return(inverx)
      }
      data <- x$get()
      inverx <- solve(data, ...)
      x$setinverx(inverx)
      inverx
}
