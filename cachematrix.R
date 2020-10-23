## The two functions below create a special object that stores
## a matrix and caches its inverse.

## makeCacheMatrix makes a list containing functions which
## set the value of the matrix, get the value of the matrix,
## set the inverse of the matrix, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## From the previous list, the function returns the inverse 
## if already cached. Otherwise, it computes the inverse and 
## caches it in the list.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
}