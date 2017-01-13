
makeCacheMatrix <- function(x = matrix()) {
  my_inverse <- NULL
  setmatrix <- function (y){
    my_matrix <<-y
    my_inverse <<- NULL
  }
  getmatrix <- function() my_matrix
  setinverse <- function (i) my_inverse <<- i
  getinverse <- function() my_inverse
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse = setinverse, getinverse = getinverse)
  }

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    my_inverse <- my_matrix$getinverse ()
    if (!is.null(my_inverse)) {
      return (my_inverse)    
    }
    data <- my_matrix$getmatrix()
    my_inverse <- solve(data)
    my_matrix$setinverse (my_inverse)
    my_inverse
  }
