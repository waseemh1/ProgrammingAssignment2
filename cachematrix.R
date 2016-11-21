##This function, makeCacheMatrix,   creates a special Matrix, which performes the
##following:

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
  {
  inv <- NULL
  set <- function(y) 
      {
      x <<- y
      inv <<- NULL
      }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }


##is function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed) then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) 
  {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
      }
      mat <- x$get()
      inv <- solve(mat,... = )
      x$setinverse(inv)
      inv
  }
