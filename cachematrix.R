## This set of functions can be used to calculate the inverse of a matrix,
## and store that inverse in an environment different from the current environment.
## This allows us to use the stored inverse instead of calculating it again,
## if we are studying the same matrix.


## makeCacheMatrix() returns a list of 4 functions: set() to set the matrix we are using
## (and reset the inverse), get() to get the matrix, setinv() to set the stored value
## of the inverse, and getinv() to call the stored value of the inverse.

makeCacheMatrix <- function(x = matrix()) 
{
      i <- NULL
      set <- function(y)
      {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve() first checks to see if the inverse of the matrix x has already been stored.
## If so, it returns that inverse.  If not, it uses solve() to calculate the inverse, stores
## that in the environment of makeCacheMatrix(), and returns the inverse.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      if(!is.null(i)) 
      {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
