#TASK OF THIS WEEKS ASIGNMENT:
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.


# Writing code for a function called "makeCacheMatrix" -> As it would mean in German (I am german): "Mache eine Sicherung deiner Matrix"
# If you have a matrix you need to store this matrix to cache in order to save it from garbage
# That's why the function is creating an other matrix (special one) that can cache the original matrixs inverse
makeCacheMatrix <- function(x = matrix()) {
  # NULL means that there is no default value
  its_my_matrix <- NULL
  
  # The following sets the old "value" of 'x' to the variable 'y' --> = Storing the given matrix to cache
  # The "value" of inverse is reset because it is not longer valid. It is now "NULL" / no default value
  set <- function(y) {
    x <<- y
    its_my_matrix <<- NULL
  }
  
  # I need to get the output on the function operating on x (= value of the matrix)
  get <- function() x
  
  
  # I need to inverse the matrix
  inverse_my_matrix <- function(inverse) its_my_matrix <<- inverse
  
  # Now what is the new value of the inversed matrix?
  get_my_inversed_matrix <- function() its_my_matrix
  
  # Make a list of all the functions associated, when the function makeCacheMatrix is performed
  list(set = set, get = get, inverse_my_matrix = inverse_my_matrix, get_my_inversed_matrix = get_my_inversed_matrix)
}



# The following function calculates/computes an inverse matrix by the output created within the makeCacheMatrix function.
# cacheSolve checks if the variable already is cached. If it does (value not NULL) it will not recalculate the inverse
# it will take it from the cache
# --> cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  # I need to get the value of my matrix (which is the already inverted-cache-saved matrix)
  its_my_matrix <- x$get_my_inversed_matrix()
  
  # If the matrixs' inverse has already been calculated --> cacheSolve needs to read data from cache
  if(!is.null(its_my_matrix)) {
    message("getting cached data")
    return(its_my_matrix)
  }
  
  # If value is NULL (matrix changed for example) --> calculation can't be taken from cache
  data <- x$get() 
  
  # The function "solve" is used - it works with a default matrix as for example the identity matrix - needed for calculation
  # Solve: "This generic function solves the equation a %*% x = b for x, where b can be either a vector or a matrix."
  its_my_matrix <- solve(data, ...) 
  x$inverse_my_matrix(its_my_matrix)
  its_my_matrix
}
