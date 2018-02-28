## Coursera - R Programming Programming Assignment 2
## write a pair of functions that cache the inverse of a matrix
## makeCacheMatrix caches the inverse of a matrix
## This function has three internal functions, to get the initial matrix
## Then setinver and getinver set the inverse and get the inverse of that matrix, respectively

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


## This function checks to see if the value is already cached and if so, returns the inverse
## If it is not cached, the inverse is calculated 

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
