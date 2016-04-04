# This matrix will hold several values including a matrix
# and an inversed matrix
#it requires a matrix and you can 1) set the matrix
#2) get the matrix 3) settheinverse 4) get the inverse 

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 
# checks to see if the inverse already exists
# If not, it returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) { 
  inv <- x$getinverse() 
  if(!is.null(inv)) { 
    message("getting cached data.") 
    return(inv) 
  } 
  data <- x$get() 
  inv <- solve(data) 
  x$setinverse(inv) 
  inv 
}


