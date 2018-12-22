## makeCacheMatrix creates a matrix and function to set the value 
## of the matrix, get the value of the matrix, set the inverse 
##value of the matrix and get the inverse value of the matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
   setInvertedMatrix <- function() m <<- solve(x)
   getInvertedMatrix <- function() m
   
   list(set = set, 
        get=get, 
        setInvertedMatrix=setInvertedMatrix, 
        getInvertedMatrix=getInvertedMatrix)
  }


## cacheSolve calculates the inverse of the matrix created with the first function
## but first checks to see if the inverse has been calculated.  If it has, it uses
## the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getInvertedMatrix()
   
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
     
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setInvertedMatrix(m)
   m
  }
