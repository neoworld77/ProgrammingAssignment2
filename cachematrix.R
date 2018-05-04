## The makeCacheMatrix function creates a list, which contains functions that:
# 1 - set the value of a matrix;
# 2 - get the value of a matrix;
# 3 - set the inverse of a matrix;
# 4 - get the inverse of a matrix; 


makeCacheMatrix <- function ( x = matrix ()) {
  
  i <<- NULL # sets inverse to NULL for future values
  
  set <- function(y) {    # function which assigns matrix x to a new matrix y
    x <<- y
    i <<- NULL        #resets the inverse
  }
  
  get <- function()x  #returns matrix x
  
  setinverse <- function(inverse) i <<- inverse  # assigns inverse to i
  getinverse <- function()i   #returns inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # This is a list containing our functions
    
}

## The cacheSolve function computes the inverse of a matrix (provided that it is invertible)

cacheSolve <- function(x, ...) {
  
  i <<- x$getinverse()        # assigns inverse of matrix x to i
  
  if (!is.null(i)) {          #conditional statement that returns i in the event that the value has already been computed and stored in the cache
    message("Getting cached data")
    return(i)
  }
  
  data <- x$get()      #assigns matrix x to data
  i <- solve(data, ...)  # assigns inverse of 'data'matrix to i
  x$setinverse(i)        # sets inverse of matrix
  return(i)              # returns inverse
  
}
