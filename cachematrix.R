# **Introduction**

## The script consist of two functions, makecachematrix and cachesolve. makecachematrix takes one input square matrix and returns 4 subset of the objects(get,set,getmatrix,setmatrix) while cacheSolve takes makecachematrix returned object as input and returns cached output if computation is done already if not performs inverse operation on the matrix, cache the result and returns the result


### makeCacheMatrix function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) inv <<- solve
  getmatrix <- function() inv
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


### cacheSolve function computes the inverse of the special matrix object returned by makeCacheMatrix function defined above, the input matrix should be a square matrix to perform inverse operation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("Getting Cached Data ...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmatrix(inv)
  inv
}
