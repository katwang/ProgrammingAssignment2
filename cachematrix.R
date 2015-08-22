## Together, the functions create an object that stores a matrix and caches its inverse


## This function builds a special "matrix" that can store its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<-NULL
  }
  get <- function() x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function() inv
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)

}


## This function finds the inverse of the "matrix" from the first function
## If the inverse of the same matrix had previously been calculated, it will take the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

