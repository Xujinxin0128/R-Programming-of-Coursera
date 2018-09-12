## This function will cash the matrix and its' inverse

## Firstly I want to assign function to makeCacheMatrix

makeCacheMatrix <- function(m = matrix) {
  i <- NULL
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  ##way to get the matrix
  get <- function() m
  ##way to set the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  ##way to get the inverse matrix
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##assign function to m
m <- makeCacheMatrix()
##then we can set and get the inverse matrix
m$setinverse( matrix(c(1,2,3,4), nrow = 2, ncol = 2))
m$getinverse()

##then I want to build cacheSolve function
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  ##we set inverse to our matrix
  x$setinverse(m)
  m
}
##the we can test it
cacheSolve(m)
##getting cached data
########[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##that is what we receive; now this data is in cache
