## makeCacheMatrix: This function creates a special �matrix� object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function(){
    x
  } 
  setInverse <- function(solveMatrix){
    inv <<- solveMatrix
  }
  getInverse <- function(){
    inv
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInverse(inverse)
  inverse
}


