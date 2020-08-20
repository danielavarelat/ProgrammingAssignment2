## Put comments here that give an overall description of what your
## functions do

## Creates a matrix special object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x ##get matrix
  setinv <- function(solve) inv <<- solve ##set inverse matrix
  getinv <- function() inv ##get inverse
  ## retorn list of 4 elements
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Receives the object created with the function above and calculates the inverse if it hasnt been already calculated, otherwise, just returns it. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## if the inverse exists, find in cache and return
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if not, calculate and save
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

###PROOF
gg<-makeCacheMatrix(A)
cacheSolve(gg)
