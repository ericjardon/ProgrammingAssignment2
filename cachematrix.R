## makeCacheMatrix is a function that creates a special matrix with get and set methods.
## solveCache is a function similar to the built-in solve() function but saves unnecessary computation.

## makeCacheMatrix takes in a normal matrix
## creates an object that is able to store the matrix (x) and its inverse (inv)
## Both the matrix and its inverse have get and set methods in this wrapper object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(newMatrix) {
    x <<- newMatrix
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverseM) inv <<- inverseM
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve works like a special solve() function,
## takes in one of the special matrices created by makeCacheMatrix
## computes the inverse only if it has not been calculated
## saves unnecessary computation if there is already an inverse

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    theMatrix <- x$get()
    inv <- solve(theMatrix, ...)
    x$setInverse(inv)
    inv
}
