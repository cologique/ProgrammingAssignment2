## makeCakeMatrix is an object (stored as a list, names=methods) that stores a matrix with a placeholder for the inverse of that matrix.  
## cacheSolve either computes that inverse and stores it in the cacheMatrix object or
## it retrieves the inverse held by the cacheMatrix object.

## TODO check that invX is really the inverse of x; this version allows anything
##      to be stored as the inverse
## 


makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(xinv) {
    invX <<- xinv
  }
  
  getInverse <- function() invX
  
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  toInvert <- x$get()
  m <- solve(toInvert)
  x$setInverse(m)
  m
}

## This function creates a cached matrix object
## which stores a matrix and (possibly) its inverse, 
## with public functions 
## initialised by a matrix object
## set(y) -- to change the internal matrixto matrix y
## get()  -- to get theinternal matrix
## setInverse(invx) -- to store the inverse of the matrix
## getInverse()  - to retrieve the current value  of the inverse (NULL if it has not been assigned yet)

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL         ## initialise inverse as NULL to indicate we have not computed it yet
  
  set <- function(y) { ## change internal matrix
    x <<- y
    invX <<- NULL      ## this needs resetting as the old value will be wrong
  }
  
  get <- function() x  ## retrieve the internal matrix
  
  setInverse <- function(xinv) {  ## store the inverse
    x<-invX <<- xinv
  }
  
  getInverse <- function() invX  ## retrieve the current value of the inverse
  
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## This function finds the inverse of a cachedMatrix object
## ie it retrieves the inverse if it has been cached
## otherwise it computes the inverse and caches it for future retrieval

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()     ## retrieve current value
  if (!is.null(m)) {      ## if not NULL we have the inverse already, so return it
    message("getting cached data")
    return(m)
  }
  ## otherwise we compute the inverse 
  toInvert <- x$get()      ## of the matrix stored in x
  m <- solve(toInvert)
  x$setInverse(m)          ## store (cache) the inverse for future use
  m                        ## return the inverse
}