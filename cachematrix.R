## This function is used to set a matrix and return a list of functions that could
## be called on the OBJECT created by makeVector. The setMatrix function will set a
## matrix if it is different from a stored matrix and INVERTIBLE. If setMatrix is 
## called the first time, it creates a tempMatrix and also assignes the new matrix 
## to the variable x. The tempMatrix is used to make sure that the same matrix is 
## not used to replace an existing matrix of the same values. This will also mean
## that the calculated INVERSE of the matrix is not recalculated in cacheSolve function.
## This function does not allow an INVERSE to be set by a user. It is set in cacheSolve.
## 
## If a user tries to set a Singular matrix, the fuction would not allow this and it
## retains the previous matrix.
##                              How to use
## 1. Assigne makeCacheMatrix to a variable. For example v <- makeCatcheMatrix()
## 2. Create a square matrix and use V$setMatrix(<your_invertible_matrix) to set the matix
## 3. Call cacheSolve function with variable 'v' to create the INVERSE of your Matrix.
##    For example, cacheSolve(v). 
##
## Written by: Kenneth Ishionwu
## Date      : June 21, 2014
## Coursera Peer Assessement assignment by Dr. Roger Peng of Johns Hopkins University

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL  # set the inverse Matrix to NULL
  setMatrix <- function(in_matrix){
      # Create a temporary matrix that is used to compare with any input
      # matrix to help not recalculate the INVERSE of already seen matrix.
      tempMatrix <- in_matrix
      if (!identical(tempMatrix, x)){
        detMatrix <- det(in_matrix)
        if(detMatrix == 0){
          message("The supplied Matrix is not INVERTIBLE and an INVERSE could")
          message("not be calculated therefore, this matrix is not set")
          tempMatrix <<- x  # store the original matrix in tempMatrix
        }else{
          x         <<- tempMatrix
          invMatrix <<- NULL
          isSet     <<- F 
        }  
      }else{
        message("This New Matrix is identical to previously saved Matrix")
      }    
  }
  
  getMatrix    <- function(){
    if(is.null(x)){
      message("No matrix is set")      
    }else{
      x
    }  
  } 
  
  
  ## This is to prevent a user from using setInvMatrix function to set
  ## an abitrary INVERSE for any matrix. The only way to set an INVERSE is
  ## at cacheSolve. That is, within the function only.
  
  setInvMatrix <- function(z){
    if(isSet){
      invMatrix <<- z
    }else{
      message("You are not allowed to set INVERSE of a Matrix")
    } 
  } 
  getInvMatrix <- function()invMatrix
  list(setMatrix = setMatrix,
       setInvMatrix = setInvMatrix,
       getMatrix = getMatrix,
       getInvMatrix = getInvMatrix)  
}

##  This function is to return the INVERSE of invertible matrix that was
##  set using makeVector function. The makeVector function enforces the
##  rule that an INVERSE MATRIX is set to NULL if the current matrix is
##  not the same as the previously seen matrix. This helps prevent 
##  cacheSolve function from recalculating a stored INVERSE Matrix.
##  The argument 'x' is an OBJECT created by assigning the makeVector function
##  to a variable and the list of functions returned by makeVector are used
##  get and set the the object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInvMatrix()
  if(!is.null(invMatrix)) {
      message("Retrieving cached Inverse Matrix")
      return(invMatrix)
  }
  data <- x$getMatrix()
  invMatrix <- solve(data, ...)
  isSet <<- T  ## After the inverse is calculated, set isSet Variable to TRUE.
  x$setInvMatrix(invMatrix)
  isSet <<- F  ## After setting the invMatrix, setit to False to not allow 
               ## setting from outside this function.
  invMatrix  
}
