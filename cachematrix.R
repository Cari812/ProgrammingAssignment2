## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #initialize variable inv property
  inv <- NULL
  #method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #method to get the matrix
  get <- function() x 
  # method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  #method to get the inverse of a matrix
  getInverse <- function() inv
  #return a list of the method implements
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#cacheSolve(my_matrix)If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  #if inverse of the matrix is not null then return its value from cache.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if inv is null, get the matrix from our objects
  matrix_ <- x$get()
  #compute the inverse of x and set the var inv with it
  inv <- solve(matrix_, ...)
  #method set the inverse to the object
  x$setInverse(inv)
  #return the matrix
  inv
}
