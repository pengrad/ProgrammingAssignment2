## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
## Return cached value if it is, otherwise calculate it, cache and return

cacheSolve <- function(x, ...) {        
  s <- x$getsolve()
  if(!is.null(s)) {
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
