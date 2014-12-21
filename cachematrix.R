##  cachematrix.R
##  Roy Lee
##  21 DEC 2014
##  R Programming
##  Programming Assignment 2: Lexical Scoping

##  This is a module for computing the inverse of a matrix.
##  Usage:
##    myMatrix <- makeCacheMatrix(<square invertible matrix (default:
##      empty matrix)>): constructor
##    myMatrix$set(<square invertible matrix>): sets the matrix
##    cacheSolve(<myMatrix>): gets the inverse using a cache
##    myMatrix$get(): returns the matrix
##    myMatrix$setinverse(inverse): sets the inverse
##    myMatrix$getinverse(): gets the inverse

##  This function creates a special "matrix" object
##  that can cache its inverse.
##  Assumption: the matrix supplied is always square invertible.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##  This function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ##  Return a matrix that is the inverse of 'x'.
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
}

##  eof
