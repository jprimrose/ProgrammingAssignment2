## James Primrose                  
## cacheMatrix.R                            '_' 
## 10.09.2018                              {0,0} - w00t!
##                                         (|_(\
##                                         -"-"-
## function that is able to cache potentially time-consuming 
## Matrix computations

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## function that is able to cache potentially time-consuming 
## Matrix computations
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## TESTING BITS BELOW
#####

# a = matrix( 
#  c(2, 4, 3, 1, 5, 7, 5, 9, 5), # the data elements 
#  nrow=3,              # number of rows 
#  ncol=3,              # number of columns 
#  byrow = TRUE)        # fill matrix by rows 


#cm <- makeCacheMatrix(a)
#cacheSolve(cm)
#cacheSolve(cm)

