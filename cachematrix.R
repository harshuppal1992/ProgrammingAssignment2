## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #set the inverse initially
  m <- NULL
  # set the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  #get the matrix 
  # get() function returns the matrix x
  get <- function() x
  
  #set inverse of the matrix:
  setinverse <- function(inverse) m <<- inverse
  
  #get inverse of the matrix
  getinverse <- function() m
  
  # create function list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Return inverse of matrix x
  
  m <- x$getinverse()
  
  # check available results:
  if(!is.null(m)){
    message("getting cached data")
    return(m)
    
    #If no available result
    data <- x$get()
    
    m <- solve(data, ...)
    
    #set inverse
    x$setinverse(m)
    
    #return inverse
    m
  }
}
