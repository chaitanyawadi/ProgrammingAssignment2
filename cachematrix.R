# This cachematrix.R file contains two functions -
# 1. makeCacheMatrix and 2. cacheSolve
 
# The first function "makeCacheMatrix" takes a matrix as 
# an input and stores a list of 4 functions( details explained below) 

# The second function "cacheSolve" perfors two tasks -
# It first checks if the inverse of the matrix is already
# calculated and returns that value 
# Else it calculates this inverse of matrix and stores it 
# in the cache so that if cacheSolve is called again,
# it returns this value 



# The four functions stored as a list by "makeCacheMatrix" function
# are 1. set 2. get 3. setinverse and 4. getinverse

#To use any one of these four functions, we need to subset the 
# main function as makeCacheMatrix$(insert function name here)

# get is a function that returns the matrix x stored in the main function

# set is a function that changes the matrix stored in the main function
# X <<-y substitutes matrix x with y in the parent i.e main function
# inv <<- NULL returns value of inverse to null

# setinverse and getinverse are similarly used to store value
# of inverse in the main function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# cacheSolve first verifies the value of inv stored previously with 
# getinverse and is not NULL and then displays "getting cached data"
# and returns previously stored inverse 

# If it is NULL, it calculates the inverse using Solve(X)
# and assigns this value to inv to be returned in case
# cacheSolve is recalled

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv

}
