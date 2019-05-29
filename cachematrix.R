## Programming Assignment 2 - R Programming
## 
## Two functions that compute the inverse of a matrix and cache the result so that, if the calculation is
## attempted again over the same variable, R first checks if the result has been cached. If that's the case, a message 
## is returned together with the inverse of the matrix.
## 
## makeCacheMatrix defines a list of functions to get and the set the values so that cacheSolve can verify 
## if a previous result has been cached. A list of funcions get, set, setinv and getinv is the stored output
## of the function.
##
## N.B. in order to test the code it is necessary to:
## a) create a (square) matrix [like in mat <- matrix(1:25, ncol = 5, nrow = 5)]
## b) use (e.g.): mylist <- makeCacheMatrix(mat) to create the list of functions
## c) and then: cacheSolve(mylist) to get the inverse of the matrix +
## c1) again cacheSolve(mylist) to check that the message "getting cached data" is returned.

makeCacheMatrix <- function(x = matrix()) {
     mtrx <- NULL
     set <- function(y) {
          x <<- y
          mtrx <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) mtrx <<- solve
     getInverse <- function() mtrx
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## cacheSolve then checks if there is a value stored and retrieves it, otherwise it computes the inverse

cacheSolve <- function(x, ...) {
     
     mtrx <- x$getInverse()
     if(!is.null(mtrx)) {
                    message("getting cached data")
                    return(mtrx)
     }
     data <- x$get()
     mtrx <- solve(data)
     x$setInverse(mtrx)
     mtrx
     
}
