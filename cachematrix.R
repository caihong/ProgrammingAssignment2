##Module to caculate an inverse of a matrix with result cached

## Function makeCacheMatrix creates a Cachemartix vector that stores matrix and its inverse with getter and setter
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##Function cacheSolve - get inverse of a matrix x either by reading cache or new calculation. 
## In the latter case it will also store the result in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)){
       message("getting data from cahce")
       return(inv)
     }
     inv <- solve(x$get())
     x$setinv(inv)
     inv
}
