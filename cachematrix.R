makeCacheMatrix <- function(x = matrix()) {   #Function to make a matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                           #getting matrix
  setinv <- function(inverse) inv <<- inverse   #setting inverted matrix
  getinv <- function() inv                      #getting inverted matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
cacheSolve <- function(x, ...) {     ##Function for getting the inverse of the matrix.
  inv <- x$getinv()                  ##If the inverse was calculated before then this function will retrieve it from cache
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
