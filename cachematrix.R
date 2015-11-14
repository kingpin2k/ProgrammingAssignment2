# this function essentially wraps a matrix in another object that exposes the
# matrixes data and solved value, personally I'd rather have the getsolve
# calculate the solution and cache at that point if it were null, but
# the assignment didn't ask for that
makeCacheMatrix <- function(x = matrix()) {
  solved <- NULL
  # store the matrix, and invalidate the solution
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  
  # matrix getter
  get <- function() x
  # solved matrix setter
  setsolve <- function(solve) solved <<- solve
  
  # solved matrix getter
  getsolve <- function() solved
  
  # list of methods
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# This follows a computed property pattern, lazily calculated pattern.  
# essentially when you attempt to find the solution of our special matrix
# it will attempt to use the cached value, and if it doesn't exist yet
# it calculate, and return the newly cached value
cacheSolve <- function(x, ...) {

  # get the current solution
  solved <- x$getsolve()
  
  # not solved
  if(!is.null(solved)) {
    return(solved)
  }
  
  # get matrix
  data <- x$get()
  
  # inverse matrix
  solved <- solve(data, ...)
  
  # cache matrix
  x$setsolve(solved)
  solved
}

