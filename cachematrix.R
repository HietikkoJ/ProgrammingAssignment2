## These pair of functions will solve the inverse of input matrix and use cached data, if the
## inverse had been calculated prior
#
#
#
# Input example:
#
# matrix1 <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# cacheSolve(matrix1)
# cacheSolve(matrix1)  #This second command will return the cached data
#
#






## This function will make special list from input matrix. The list have holders for each function
## for setting and getting the orginal matrix and also it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## This function will solve the inverse from input matrix. If the inverse is already solved,
## it returns the cached data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
