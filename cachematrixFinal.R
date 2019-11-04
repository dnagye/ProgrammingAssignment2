makeCacheMatrix <- function(x = matrix()) { #builds a set of functions: set(), get(), setinverse(), getinverse() within a list to the parent environment 
  i <- NULL
  set <- function(y) { #assigns the input argument to the x object in the parent environment
    x <<- y
    i <<- NULL #assigns the value of NULL to the i object in the parent environment.
  }                                               
  get <- function() x #retrieves x from the parent environment of makeCacheMatrix()
  setinverse <- function(inverse) i <<- inverse # defines the setter for the inverse i
  getinverse <- function() i #defines the getter for the inverse i
  list(set = set, get = get, #assigns each of these functions as an element within a list()
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) { #retrieve the inverse from makeCacheMatrix()
  i <- x$getinverse() #retrieve an inverse from the object passed in as the argument
  if(!is.null(i)) { #checks to see whether the result is NULL
    return(i)
  }
  data <- x$get() #gets the matrix from the input object
  i <- solve(data, ...)
  x$setinverse(i) #set the inverse of the input object
  i
}

B <- matrix(c(1,2,3,4),2,2) #example invertible matrix
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
