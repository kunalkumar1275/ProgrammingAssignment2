## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Declaring inverse & taking matrix input in variable x
  inverse <- NULL
  set_matrix <- function(y){
    x <<- y
    inverse <<- NULL	
  }
  
  ## Function to retrieve matrix
  get_matrix <- function(){
    x
  }
  
  ## Function to set inverse of matrix
  set_inverse <- function(inverse_data){
    inverse <<- inverse_data
  }
  
  ## Function to retrieve inverse of matrix
  get_inverse <- function(){
    inverse
  }
  
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## getting inverse of matrix from previous functions
  inverse <- x$get_inverse
  
  ## Checking if inverse is NULL or Not
  if(!is.null(inverse)){
    print("Getting cached data")
    return(inverse)	
  }
  
  ##In case Inverse is NULL, Inverse gets calculated
  data <- x$get_matrix
  data1 <- solve(data)
  
  ## Setting value of inverse in previous function
  x$set_inverse(data1)
  data1
  
}
