## The purpose of thses functions is to save time and reduce errors on/in computations. By first computing the inverse
## of a matrix and cacheing the values we can then look up the result so we dont have to compute it every time


## The makeChachematrix function creates a matrix that will cache its inverse. There are four subfunctions that 
## make up the whole function: set (matrix), get (matrix),  setInv (set inverse), and getInv (get inverse).

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y 
    mInv <<- NULL
  }
  
  get <- function() x 
  setInv <- function(inverse) mInv <- inverse
  getInv <- function () mInv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve is a function that will compute the inverse of the matirx returned from makeCachematrix or if the 
## inverse is already computed it returns the matrix from the cache 

cacheSolve <- function(x, ...) {
  mInv <- x$getInv()
  if (!is.null(mInv)) {
    message("hold on")
    return(mInv)
  }
  
  m <- x$get()
  mInv <- solve(m, ...)
  x$setInv(mInv)
  mInv        ## Return a matrix that is the inverse of 'x'
}

