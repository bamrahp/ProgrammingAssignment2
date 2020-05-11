#Two functions have been defined below that cache the inverse of a matrix

#The first function, makeCacheMatrix creates a special matrix object. 
# It is a list containing functions to get and set the matrix object 
# And also functions to get and set the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  #Initialize the inverse placeholder and sets the value to NULL
  m <- NULL
  
  #This function is used to set the value of the Matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Returns the Matrix Value
  get <- function() x
  
  #Function to set the inverse of matrix
  setInverse <- function(inverseValue) m <<- inverseValue
  
  #Returns the Matrix Inverse
  getInverse <- function() m
  
  #Returns a special matrix object that is a list of the functions defined above
  list(set = set, get = get,
       setInverse= setInverse,
       getInverse = getInverse)
}

#The below function computes the inverse of the special matrix created with the above function. 
#First it checks if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data using solve function and sets the value of inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  #Fetches the inverse of the Matrix
  m <- x$getInverse()
  
  #Verifies that inverse is not NULL. if not NULL it returns the stored value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #If inverse is not stored in cache it computes it below
  #Get matrix
  data <- x$get()
  
  #Compute inverse of the Matrix
  m <- solve(data)
  
  #Sets the inverse to the matrix object
  x$setInverse(m)
  
  #returns the value
  m
}
