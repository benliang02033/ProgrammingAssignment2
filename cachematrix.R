## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<-function(y){          #set the value of the vector
    x <<- y
    m <<- NULL
  }
  get <- function() x                                #get the value of the vector
  setinverse <- function(inverse) m <<- inverse      #set the value of the mean
  getinverse <- function() m                         #get the value of the mean
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                 # get the invserse of m, and check if it is already stored in the cache
  if(!is.null(m)){                 
    print("Getting the cache inverse.")
    return(m)
  }
  data <- x$get()                    # The matrix m doesnt have a existing inverse, so compute the inverse of m.
  m <- solve(data, ...)
  x$setinverse(m)
  m                                  # Return the inverse of m.
}
