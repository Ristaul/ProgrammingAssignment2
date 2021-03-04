# This is a pair of functions, where the first allows you to recall (get) an inverse from a list, create a cache (set), and store cache matrix inverse values in a list

## This function  takes an argument (a matrix) that are really the inverse values from the cache solve function (next one). The other thing
## is that this function will store that matrix object (the function argument) so that it can be used to recall, and produce solutions quicker than needing to constantly calculate the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m 
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}


## This function leverages the previous function by checking to see if a matrix inverse exists, and if it does, 

## it provides a response (w/ a message), and if not, it produces the matrix inverse (leveraging solve function)
## and stores it back in the list from the previous function using the 'set' capabilities.


cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  Matrx <- x$get()
  m <- solve(Matrx,...)
  x$setInv(m)
  m
  
}
