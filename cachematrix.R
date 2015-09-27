## The goal for these two functions is to improve efficiency while calculating
## the inverse of a Matrix; It will cache a result of an existent matrix and return it


## This function builds the structure to store and get the inverse.
## It should be called before cacheSolve with a squared matrix, because it will return a list 
## that can be used to subsequent calculate the Inverse, while allowing the use of the $ operator
## Parameters: x - class matrix, the actual matrix that will be used in your algorithm
## Returns: list - representing the object matrix and the functions get and set
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(solve) m <<- solve
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
  
  
  
}


## The function performs the checking for an existent environment matrix and also calculates the 
## inverse
## Returns: matrix
## Parameters: The list returned by makeCacheMatrix
## Example: cacheSolve(makeCacheMatrix(yourMatrix))
cacheSolve <- function(x, ...) {
        
  
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInverse()
  
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
  
}
