#The function, makeCacheMatrix creates a special "matrix", which contains a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of matrix
#get the value of the inverse of matrix

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

# The function cacheSolve calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if theinverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverx function.

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
