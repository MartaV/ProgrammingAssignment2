## Put comments here that give an overall description of what your
## functions do

## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This is a function that takes a matrix 'x' as argument and can cache its inverse:
#  It keeps track of the inverse of matrix 'x': more explicitly, of whether
#  or not its inverse has been computed. If it has been computed, it caches
#  the inverse and is able to return said cache to the user (hence avoiding
#  the repeat of now unnecessary computation).
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {   
    x <<- y                  # x becomes the input matrix y
    inverse <<- NULL         
  }
  getMatrix <- function() x                             # returns matrix x
  setInverse <- function(inverse) inverse <<- inverse   # caches inverse
  getInverse <- function() inverse                      # simply returns the inverse
  s = list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}

## This function returns a matrix that is the inverse of input matrix 'x'.
#  It first checks if the object containing the inverse of matrix 'x' is 
#  NOT empty: if it indeed is not empty, 'inverse' simply grabs the value
#  from the cache. If it is empty, it computes the value then caches it.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()                          # calling getInverse function 
  if(!is.null(inverse)) {                          # i.e. if there is an inverse already cached...
    message("Getting cached inverse.")
    return(inverse)                                # .. we simply attain it via return from makeCacheMatrix that stores it
  }
  matrix <- x$getMatrix()                          # if there is no inverse, we read in the matrix..
  inverse <- solve(matrix)                         # .. we calculate it
  x$setInverse(inverse)                            # .. and cache it for future use.
  inverse
}


##   Example with a friendly, square, 2x2 matrix:
# > testMatrix = matrix(c(1,2,3,4), nrow = 2, ncol = 2)

## Making a cacheMatrix out of our testMatrix:
# > cacheMatrix <- makeCacheMatrix(testMatrix)
## Checkin for existence of inverse:
# > cacheMatrix$getInverse()
# NULL       

## So as of now, there is no inverse yet. No we can call
## cacheSolve on cacheMatrix to calculate the inverse:
# > cacheSolve(cacheMatrix)      
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## Again, we try to call getInverse to see if there exists
## an inverse now:
# > cacheMatrix$getInverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
## Indeed, the inverse is has now been set in makeCacheMatrix 
## function and can be recalled as needed
