## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL  # initializes a NULL to eliminate values remaining from some previous run
      set <- function(y) { # setter function written to make later usages for the x arguement values 
                           # (i.e. "y") so that it can edit x in the global environment and assign it
                           # new values if needed. If this doesn't happen, the same "x" entered in the
                           # arguement will remain.
            x <<- y
            inv <<- NULL   # resets any previous answer, so that the new matrix's answer can be filled here
      }
      get <- function() x  # getter function; it returns the matrix that was determined by the setter
      
#initalizing a setter function for later usage (as in the cacheSolve() function, also allows for editing)
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv #initalizing a getter function for later usage that checks for NULLs or 
                               #previous cached inverses
      
      #returns a list of objects in the environment.
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## Write a short comment describing this function
## Returns a matrix that is the inverse of 'x' either by indeed solving it, or getting it from the cache
cacheSolve <- function(x, ...) {

      inv <- x$getinv() #returns the entered matrix from the makeCacheMatrix() function
      if(!is.null(inv)) { # condition for returning only existing values
            message("getting cached data")
            return(inv)
      }
      #getting the matrix
      data <- x$get()
      #inversing the matrix
      inv <- solve(data, ...)
      #assigning the inversed matrix to the inversed-matrix-setter
      x$setinv(inv)
      #returns inversed-matrix-setter (technically, the solution)
      inv
}
