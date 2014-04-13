##
## Filename: cachematrix.R
##
## Description:
## Implement a pair of functions for creating and using a special
## matrix object capable of computing and caching its inverse.


## Function: makeCacheMatrix
## Create a special matrix object that will cache its inverse.
## Arguments:
## x - optional argument of an R matrix
## Returns:
## Special matrix object capable of caching its inverse.
## 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  # assign a new R matrix, new_x, to our special matrix
  set <- function(new_x) {
    x       <<- new_x 
    inverse <<- NULL
  }
  
  # acquire existing R matrix
  get <- function() x
  
  # set the inverse of the R matrix to the given
  # value
  setinverse <- function(inv) inverse <<- inv
  
  # acquire the inverse of the matrix
  getinverse <- function() inverse
  
  # return list of exposed functions as our
  # caching matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse= getinverse)
}

## Function: cacheSolve
## Return the inverse of the supplied matrix.
## Arguments:
## x - special matrix capable of caching its inverse;
##     use makeCacheMatrix() to obtain.
## Returns:
## The inverse of the supplied matrix.
##
cacheSolve <- function(x, ...) {

  # get current inverse (may be null)
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    return(inverse) # return cached copy
  }
  
  # inverse is currently NULL so retrieve the  
  # underlying R matrix and compute inverse,
  # store it in our cache, and return.
  cur_matrix  <- x$get()
  inverse <- solve(cur_matrix, ...)
  x$setinverse(inverse)
  inverse
}

##
## Unit tests for our above functions (ungraded)
##

cacheMatrixUnitTests <- function() {
  
  # Test the makeCacheMatrix API
  source("cachematrix.R")
  # Test 1: use default
  message("Test Set 1: object basics")
  message(" - 1a: create using default")
  cm <- makeCacheMatrix()
  if (is.list(cm)) {
    message(" - 1b: set function")
    if (!is.function(cm$set))        { stop("    [FAILED] - set function not implemented")        }
    message("    [PASSED]")
    message(" - 1c: get function")
    if (!is.function(cm$get))        { stop("    [FAILED] - get function not implemented")        }
    message("    [PASSED]")
    message(" - 1d: set inverse function")
    if (!is.function(cm$setinverse)) { stop("    [FAILED] - setinverse function not implemented") }
    message("    [PASSED]")
    message(" - 1e: get inverse function")
    if (!is.function(cm$getinverse)) { stop("    [FAILED] - getinverse function not implemented") }
    message("    [PASSED]")
  } else {
    stop("    [FAILED] - matrix object is not implemented as list.")
  }
  
  message(" - 1f: make using existing matrix")
  tm_in <- matrix(round(rnorm(16)*10),4)
  cm <- makeCacheMatrix(tm_in)
  tm_out <- cm$get()
  if (is.matrix(tm_out) && dim(tm_in) == dim(tm_out) && all(tm_in == tm_out)) {
    message("    [PASSED]")
  } else {
    stop("    [FAILED] - retrieved matrix not same as matrix used during make")
  }
  
  message("Test Set 2: object API calls")
  message(" - 2a: set/get calls")
  tm1 = matrix(round(rnorm(16)*10),4)
  cm$set(tm1)
  tm2 <- cm$get()
  
  if (is.matrix(tm2) && dim(tm1) == dim(tm2) && all(tm1 == tm2)) {
    message("    [PASSED]")
  } else {
    stop("    [FAILED] - retrieved matrix not same as set matrix")
  }
  
  message(" - 2b: setinverse/getinverse calls")
  inv_m1 <- solve(cm$get())
  cm$setinverse(inv_m1)
  inv_m2 <- cm$getinverse()
  if (is.matrix(inv_m2) && dim(inv_m1) == dim(inv_m2) && all(inv_m1 == inv_m2)) {
    message("    [PASSED]")
  } else {
    stop("    [FAILED] - retrieved matrix not same as set matrix")
  }
  
  message("Test Set 3: cache solve")
  
  tm1 = matrix(round(rnorm(16)*10),4)
  inv_tm1 <- solve(tm1)
  cm <- makeCacheMatrix(tm1)
  inv_tm2 <- cacheSolve(cm)
  inv_tm3 <- cacheSolve(cm)
  
  if (is.matrix(inv_tm2) && is.matrix(inv_tm3) && dim(inv_tm1) == dim(inv_tm2) && dim(inv_tm1) == dim(inv_tm3)) {
    message("    [PASSED]")
  } else {
    stop("    [FAILED] - inverse matrix from cache did not match")
  }
  
  message("DONE. All tests PASSED.")
}