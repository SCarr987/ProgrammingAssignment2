## Coursera - R Programming Programming Assignment 2
## Demonstration of lexical scoping, to use variables defined in context of an R-object function
## to decrease computing time for large objects, such as a large-dimensioned matrix
## This code demonstrates inversion and caching after the get operation
## which is available to a second function that uses the cache if available
## or performs inversion if not. 
##
## Function descriptions
## makeCacheMatrix caches the inverse of a matrix
## cacheSolve checks to see if inverse has been cached previously, if not, performs inverse
## The solve function takes the inverse of a matrix
##
## makeCacheMatrix detail 
## Contains three functions - get to get the matrix, setinver to compute the inverse
## and getinver as a handle for passing cached information to next function, cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
    set <- function(y) {
      x <<- y
      inver <<- NULL
    }
    get <- function() x
    setinver <- function(solve) inver <<- solve
    getinver <- function() inver
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
}

## cacheSolve detail
## Receives return value getinver from makeCacheMatrix
## If getinver is populated this is the cached value for matrix inverse, and this value is returned
## otherwise, the matrix inverse is calculated and this value is returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  return(inver)
}
