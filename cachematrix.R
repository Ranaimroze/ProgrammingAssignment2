## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  library(matlib) # the code is to load the library matlib as we will use inverse function below
  i <- NULL # the code just simply set make i variable stored as null
  set <- function(y) {
    x <<- y #<< this is use to make use of << this mean that x is global variable and can be use outside the function, the 
    # x is assiged value of y
    i <<- NULL  # same goes here the i is a global function and can be used outside the function
  }
  # the above is simply set the value of y and null to global variable x and i
  
  get <- function() x  # this defines the function get that simply return value of x
  setinv  <- function(inv) i <<- inv  # this defines the function seti  that take the inv and assign it to global variable i 
  getinv  <- function() i  # this defines the function get that simply return value of i
  list(set = set, get = get, # this will create a list of function set, get, setinv  , getinv  to its values
       setinv  = setinv  ,
       getinv  = getinv)
}

cacheSolve <- function(x, ...) {  
  i <- x$getinv  () # this get the inv of the matrix by calling getinv on the input x and assign it to the local variable i
  if(!is.null(i)) {  
    message("getting cached data")
    return(i)      # if the variable i is not null it will show the message getting cached data and then return i
  }
  data <- x$get()  # this get the original matrix and assign it to data
  i <- inv(data, ...) # here we will calculate the inverse of the matrix 
  x$setinv(i)  # this set the inverse of the matrix by calling the function set inv, remember setinv simply take the inv and assign it to global variable i
  i   # this will return the inverse
}

