## This file contains 2 functions, 1st functions performs 4 task as following
## i. declares a matrix ii. retrieves a matrix iii. Sets inverse of a matrix based on
## an argument iv. retrieves inverse of a matrix
##
## 2nd function calculates inverse of a function if it is not already calculated
## if inverse is already calculated, it skips calculation and returns the cached value

## Below function defines a matrix and sets its inverse based on an argument

makeCacheMatrix <- function(x = matrix()) {
  
  ## Declaring inverse as NULL
  m <- NULL
  
  set <- function(y){   
    if ( (is.null(x)) || (x != y) ) { ## Inputting y only if x is NULL or x != y
            print("input matrix is NOT same as last matrix, setting inverse as NULL")
            x <<- y                    ## Setting value of y in x
            m <<- NULL	         ## Declaring inverse as NULL for new 
    }
    else { print("input matrix SAME as last matrix, inverse is cached") }  
  }
  
  ## Function to retrieve matrix
  get <- function(){
    x
  }
  
  ## Function to set inverse of matrix based on ARGUMENT
  set_inverse <- function(inverse_data){
    m <<- inverse_data
  }
  
  ## Function to retrieve inverse of matrix
  get_inverse <- function(){
    m
  }
  
  ## Creation of a list to return a list with its arguments
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function actually computes inverse of a matrix. 
## Returns a matrix that is inverse of matrix provided by argument

cacheSolve <- function(x, ...) {

  ## getting inverse of matrix from previous functions
  m <- x$get_inverse()
  
  ## Checking if inverse is NULL or Not
  
  if(!is.null(m)){                      ## in case inverse is NOT NULL
    print("Getting cached data")
    return(m)	                          ## return cached inverse (which is not NULL)
  }
  
  ##In case Inverse is NULL, Inverse gets calculated and gets assigned to m 
  data <- x$get()
  m <- solve(data)         ## inverse gets assigned to m
  
  ## Setting value of inverse in makeCacheMatrix
  x$set_inverse(m)
  m                       ## Returns inverse from this function
  
}
