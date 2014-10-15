## This set of functions caches the result of inverting a square matrix
## 
##   Usage:
##          ## assume matrix is in the variable squareMatrix
##          ## you can create a 5x5 one with: (but it may not be solvable)
##          ## squareMatrix <- matrix(as.integer(rnorm(25,100,150)), 5)
##
##         cachedMatrix <- makeCacheMatrix(squareMatrix)
##        
##         ## then get the inverse from the cache.
##         inverse <- cacheSolve(cachedMatrix)
##

## makeCacheMatrix caches the supplied matrix and sets the invert to null
## use this if you want to cache the result of a matrix invert operation.
makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function takes a makeCacheMatrix function as input
## and uses it to cache the result of a matrix inversion so that
## the result of this expensive computation can be cached and reused
## without have to recalculate.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
      if(!is.null(inverse)) {
              message("getting cached inverse")
              return(inverse)
      }
      X <- x$get()
      inverse <- solve(X)
      x$setinverse(inverse)
      inverse
}
