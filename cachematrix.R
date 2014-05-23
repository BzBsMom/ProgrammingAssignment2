# makeCacheMatrix returns a list that cacheSolve will use to retrieve the or solve the inverse of
#the matrix passed to makeCacheMatrix
## makeCacheMatrix return a list of the functions used to set and retrieve the matrix
## as well as the functions used to set and retrieve the inverse
## Do not allow to alter the contents of the inverse once the matrix is set to avoid inconguence

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinv <- function(solve) {
            if(is.null(mi)) {
              mi <<- solve 
            }
            else
              message("cannot alter inverse")
    }
  getinv <- function() mi
  list(set = set, get = get,
       setinverse = setinv,
       getinverse = getinv)
}


## Receives a matrix referenced by the list from makeCacheMatrix and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getinv()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()

  mi <- solve(data,...)

  x$setinv(mi)
  return(mi)
}
