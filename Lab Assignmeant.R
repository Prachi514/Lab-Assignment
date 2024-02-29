# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(matrix) {
    mat <<- matrix
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve function computes the inverse of the special "matrix" and caches it
cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  matrix <- cacheMatrix$get()
  inverse <- solve(matrix, ...)
  cacheMatrix$setinverse(inverse)
  inverse
}

# Example usage
# Create a cache matrix
myMatrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Compute the inverse (will calculate and cache)
inverseMatrix <- cacheSolve(myMatrix)

# Retrieve the cached inverse without recalculating
cachedInverse <- cacheSolve(myMatrix)
