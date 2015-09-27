## These functions allow for the calculation of the inverse of a matrix
#and the storing of the solution to a cache variable. This allows
#the inverse of a particular matrizx to be retrived faster than recalculting 
#every time.

## This function stores the cache of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x #return matrix
    setInverse <- function(z) inverse <<- z #set value(in this case the inverse) from solve() function
    getInverse<- function() inverse # return the inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function returns the inverse of a matrix if solved,
# and if it has not been solved, then calculates the inverse and stores
#in the cache using the methods from the makecachematrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
