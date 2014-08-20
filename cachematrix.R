## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix",  which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix
##
## Walkthrough of the function and example of usage
## a <- matrix(rnorm(2000**2,10,10),2000) -- Matrix of 2000 x 2000
## c <- makeCacheMatrix(a) -- c is the special matrix created from a with the features described above.
##                            The inverse is not calculated at this point
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of 
## the data and sets the value of the inverse in the cache via the setinverse function.
##
## Walkthrough of the function and example of usage
## d <- cacheSolve(c) -- The inverse is calculated now. Please, notice how this operation takes 
##                       several seconds (around 13s in a Lubuntu VM with 3G of RAM)
## e <- cacheSolve(c) -- As the argument for the function has not changed (especial matrix c), the 
##                       inverse is retrieved from the cache. A message in the console indicate
##                       that the data comes from the cache.
##                       As the inverse is not calculated again, the function returns inmediately. 
## f <- d %*% a       -- We multiply the original matrix (a) with the inverse returned by function
##                       cacheSolve an verify that the result is the identity matrix. Therefore
##                       we conclude that the inverse was calculated correctly.    
## summary(colSums(f)), summary(rowSums(f)) -- With these to calculations we check that f is
##                       the identity matrix. The result is all 1.
## sum (abs(g <- d - e)) We check that the inverse and the cached inversed are equal. The result
##                       is 0.
## h <- cacheSolve(makeCacheMatrix(d)) We calculate the inverse of the d, which should be equal to 
##                      the original matrix a (inverse(inverse(a)=a)). This test is to check the
##                      funtion will make another calculation and not return the cached value.
## sum (abs(a-h))       We check that the inverse is calculated correctly (a = h). The result
##                      of this piece of code is 0.
  
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



