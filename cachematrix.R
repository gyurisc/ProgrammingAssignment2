## Put comments here that give an overall description of what your
## functions do

## makeCache matrix expects a matrix as input parameter and returns 
## with a list of the follwoing functions
## set: is a function that initalizes the list with y and clears the i (short for inverse)
## get: returns with the matrix stored in this list 
## setinv: sets the i to be value of inv
## getinv: returns with the value of i 
## 
## The following instructions can be used to test the program: 
## 
## - First create the structure: 
## amatrix = makeCacheMatrix(matrix(c(11,23,31,14,65,16,17,88,19), nrow=3, ncol=3))
## - Print out the matrix created, by calling the get function 
## amatrix$get()
## - Calculate the inverse of the matrix 
## cacheSolve(amatrix)
## - Print out the inverted matrix 
## amatrix$getinv()


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Receives the input parameter x and does the following: 
## - checks if the inv is already computed and if so then returns its value 
## - if the inv is not defined then calculates the inverse of the matrix stored in x 
##   and stores the newly computed value by using the setinv function 
##   return the newly computed value 

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
