## Calculation of inverse matrix using cache to speed up. 
## 

## Makes a list of functions for setting to the cache, 
## and getting from the cache a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <- function(y)  {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       getinv = getinv, setinv = setinv)
}


## Returns a inverse matrix set in the cashe. If it has been
## calculated before, returns a cached matrix. Otherwise,
## calculates the inverse anew and returnsit.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i))  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}