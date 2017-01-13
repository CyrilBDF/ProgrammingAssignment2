makeCacheMatrix <- function (x = matrix ()) {
  my_inverse <- NULL
  setmatrix <- function (y){
    x <<-y
    my_inverse <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function (i) my_inverse <<- i
  getinverse <- function() my_inverse
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function (x,...){
  my_inverse <- x$getinverse ()
  if (!is.null(my_inverse)) {
    return (my_inverse)    
  }
  data <- x$getmatrix()
  my_inverse <- solve(data)
  x$setinverse (my_inverse)
  my_inverse
}
