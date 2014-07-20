## Put comments here that give an overall description of what your
## functions do
## Nuala Davis
## Maybe by week3 I will get it
## Exercise hints incuded along with some test scenarios to work out both


## makeCacheMatrix returns 4 functions set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes as input a matrx created by MakeCacheMatrix
## cached inverse values, if present, are returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

## Testing...
## create a matrix
testmatrix<-matrix(c(3,6,1,6,9,2,3,6,4),3,3)
testmatrix
## check what the inverse should be
solve(testmatrix)

## test out revised functions
## z is our "special" self caching matrix
z<-makeCacheMatrix(testmatrix)
## computed first time
cacheSolve(z)
## fetched from cache second time
cacheSolve(z)




## and here are the bits given to us in the exercise, heavily reused!
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("retrieving stored data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## and some test data to use with this....
##Define a vector for use with starting objects
testmatrix <-c(rep(10,100)
testmatrix[2]<-700

## try out these two functions
z<-makeVector(testmatrix)
y<-cachemean(z)
y



