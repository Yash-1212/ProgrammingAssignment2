##This is a program to Cache the inverse of the matrix

##First Function: Creates a Special object, "matrix", which caches its inverse. 
CacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(j)
  {
    x <<- j
    i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


##Second Function:  This function computes the inverse of the special "matrix". If the inverse has already been calculated then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  i <- x$getInverse()
  if(!is.null(i))
  {
    message("Obtained cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i
}