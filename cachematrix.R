
# 'makeCacheMatrix' is a function that stores and displays a matrix 'x' inputted by a user as well as 
# stores and displays the cached inverse of that matrix. 'cacheSolve' computes the inverse of matrix
# 'x' specified in 'makeCacheMatrix'.


# makeCacheMatrix is a function which, when called, outputs a list of functions that exist 
# within its body. The function can set matrix as given by the 'x' argument and display that matrix.
# Furthermore, a user can also manually set the inverse of the matrix and display that inverse.
# LIST OF FUNCTIONS WITH DESCRIPTIONS:
#
# 1. 'setMatrix'  --------> sets the matrix as specified by the 'x' argument
# 2. 'getMatrix'  --------> displays the matrix set by 'setMatrix' function
# 3. 'setInverse' --------> sets the inverse of matrix (not necessarily the one specified by 'x')
# 4. 'getInverse' --------> displays the inverse of the matrix specified by 'setInverse'

makeCacheMatrix <- function(x = matrix()) {
      
      #sets the value of 'inverse' to NULL
      inverse <- NULL
      
      #stores a matrix as given by 'x'
      setMatrix <- function(y) {
            x <<- y
            
            #sets the inverse to NULL since new matrix 'x' is specified 
            inverse <<- NULL
      }
      
      #displays the stored matrix
      getMatrix <- function() x
      
      #sets the inverse of a matrix (again, not necessarily of 'x', user can input any matrix here)
      setInverse <- function(newInverse) inverse <<- newInverse
      
      #displays the inverse matrix specified by 'setInverse'
      getInverse <- function() inverse
      
      #lists all the functions within the body of the main function 'makeCacheMatrix'
      list(setMatrix = setMatrix, 
           getMatrix = getMatrix, 
           setInverse = setInverse, 
           getInverse = getInverse)
}

# cacheSolve is a function whose aim is to calculate the inverse of the matrix specified by function
# 'setInverse'. It first checks wheter the inverse already exists in memory. If yes, it displays
# a message and returns that inverse. If the inverse does not exist, it calculates the inverse of
# matrix stored by 'getInverse' and then outputs the result.

cacheSolve <- function(x, ...) {
      
      # checks whether the inverse already exists
      inverse <- x$getInverse()
      
      # if the inverse exists:
      if(!is.null(inverse)) {
            
            # displays a message: "retrieving the inverse from cache"
            message("retrieving the inverse from cache")
            
            # and outputs that inverse
            return(inverse)
      }
      
      # if the inverse does not exist:
      
      # creates a 'data' matrix to temporarily store the contents of 'getMatrix'
      data <- x$getMatrix()
      
      # calculates the inverse of the 'data' matrix
      inverse <- solve(data, ...)
      
      # sets the inverse of 'data' matrix so that it can be read by 'getInverse'
      x$setInverse(inverse)
      
      # outputs the inverse to the console
      inverse
      
}
