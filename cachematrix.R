## Put comments here that give an overall description of what your
## initiate a matrix called x and set its default value to a empty matrix. Then pass it to function makeCacheMatrix
## initiate a data object called i (for inverse) and set it to null
## set argument is to  assign the input arguement to the x object in the parent enviornment and assign null to i
## get is the getter function, set is the setter function through setinverse
## the list function assigns each of these functions as an element within the list(), and returns it to the parent environment.
makeCacheMatrix <- function(x = matrix())
{
  i<-NULL
  set<-function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inversematrix) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## cachesolve function here starts with a single argument x and allows the caller to pass on  additional arguments into the function
## the function attempts to retrieve the inveres of the matrix from the object passed in as the argument. First, it calls the getinverse() function on the input object.
## It then checks if the result is NULL. If it is not null, then return the cached i and print 'getting cached data'
## If the !is.null(i) is false, cacheSolve gets the matrix from the input object and then calculates the inverse through solve(), it then uses the setinverse() function to set the inverse matrix in the input object, and returns it to the parent enviornment

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Below is for testing to see if makeCacheMatrix and cacheSolve work
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)

## Write a short comment describing this function

