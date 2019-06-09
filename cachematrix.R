## Write a short comment describing this function
##My function will take the matrix and invert the matrix.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              
  setInverse <- function(inverse) invMatrix <<- inverse  
  getInverse <- function() invMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}



cacheSolve <- function(x, ...) {
  
  
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       
    message("Gets Invertible Matrix")  
    return(invMatrix)                             #invertible matrix return
  }
  
  
  MatrixData <- x$getMatrix()                    
  invMatrix <- solve(MatrixData, ...)             
  x$setInverse(invMatrix)                         
  return(invMatrix)                              
  
}


#TESTING OF THE FUNCTION#

TestMatrix <- matrix(2:6,2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)




