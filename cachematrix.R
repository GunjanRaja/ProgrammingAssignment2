makeCacheMatrix <- function(x = matrix()) { 

      xinv <- NULL # to store result of inversion
        set <- function(y) {
	  x <<- y
	  xinv <<- NULL 
      }

      get <- function() x # return the input matrix
      setInv <- function(inv) xinv <<- inv # set the inversed matrix
      getInv <- function() xinv # return the inversed matrix
      	list(set = set, get = get,
	      setInv = setInv,
	      getInv = getInv)
  }


  cacheSolve <- function(x, ...) {
      m <- x$getInv() # get the inversed matrix from object x
        if(!is.null(m)) { # if the inversion result is there
	  message("getting cached data")
	  return(m) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # we solve it
      x$setInv(m) # we then set it to the object
      m # return the solved result
  }

  test <- matrix(runif(9,1,100),3,3)
  # generate the makeCacheMatrix object with this matrix
  testCached <- makeCacheMatrix(test)
  # from now on calculate or retrieve calculated inversion using the cacheSolve function

  testInv <- cacheSolve(testCached)

