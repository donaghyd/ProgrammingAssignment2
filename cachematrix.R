## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function takes a matrix as an input object and sets the value of the matrix.
#It includes functions to get the value of the matrix, set the inverse matrix, and get 
#the value of the inverse. 
#The list function allows these nested functions to be used with the $ operator.
#[ex. makeCacheMatrix(matrix(1:4, 2, 2))$getInv)]

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set value of the matrix
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x                           #get matrix value
  setInv <- function(inverse) inv <<- inverse   #set inverse value
  getInv <- function() inv                      #get inverse value
  list(set = set, get = get, 
       setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

##This function uses the output of the previous function and checks if the inverse matrix has value.
#using the !is.null function. If the inverse has no value, it gets the value using the solve() function.
#If the inverse has value, the function displays the message "getting cached data" and returns the inverse. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()                   
  if(!is.null(inv)) {                   #if inverse is not NULL   
      message("getting cached data")    #display message
      return(inv)                       #return inverse
  }
  data <- x$get()                       #get the original matrix
  inv <- solve(data, ...)               #find the inverse of the original matirx
  x$setInv(inv)                         #set as the inverse
  inv                                   #return inverse
}
