## The functions work in the same way as the makeVector() and the cachemean() functions.

## First make a random square matrix and store it in a variable (say original_matrix).
## Then pass it through the makeCacheMatrix() function and assign it to a new variable.
## Finally call the cacheSolve() function to use the inverse whenever needed 
## in the same way as the solve() function is called. 
## If the value is stored in cache...it's stored value is returned
## Else the cache is updated with the inversion matrix of the matrix passed as argument
## The outputed matrix is the inverse of the first matrix variable (in this case original_matrix).
## Output can be matrix multiplied (i.e. %*%) with the original_matrix and it should yield an identity matrix.

## This function is the function corresponding to makeVector()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function is the function corresponding to cachemean()

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
