# The aim of these two functions is to compute the inverse 
# of a matrix just once and then cache it back to the program.
# Using cached objects, the computation time can be reduced,
# especially for large operations (in this case, for large matrices
# that have to be inverted.)



## makeCacheMatrix: The function to cache the matrix and its inverse
# This function takes a matrix as input and then
# creates an object, a list with functions to: 
# set the matrix, to get it back,
# to compute its inverse and the get its inverse back.

# Define  function and arguments
makeCacheMatrix <- function(x = matrix()){ 
  # Create empty object in which the inverse is stored
  i <- NULL 
  # Create the cached objects for the matrix and its inverse
  set <- function(y){ 
    x <<- y
    i <<- NULL
  }
  # Function to call the matrix
  get <- function() x
  # Function to get the inverse of the matrix
  setinv <- function(solve) i <<- solve
  # Function to call the inverse
  getinv <- function() i
  # Create list with all four functions created
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv) 
}


## cacheSolve: The function to calculate the the inverse
# This function takes the object created by "maneCacheMatrix"
# as input. It gets the cached inversed matrix if 
# it has already been inverted once.
# If not, it computes its inverse and then stores / 
# caches it to an object, such that it can be cached
# the next time the computation is made.

# Define function and arguments
cacheSolve <- function(x, ...){
  # Get object with inverse from "makeCacheMatrix"
  i <- x$getinv()
  # If the inverse from "makeCacheMatrix" is not empty, 
  # (i.e. inverse has already been computed), return inverse
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  } 
  # If the inverse is empty, load the matrix from "makeCacheMatrix",
  # calculate its inverse and store it as object that can later be cached
  else{
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
  }
  # return inverse of matrix
  i
}

