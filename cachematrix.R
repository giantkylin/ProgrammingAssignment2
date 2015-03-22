## Function to register a invertible matrix's information
##  @x: a square invertible matrix
##  return: a list of functions
##      set:  set the matrix
##      get:  get the matrix
##      setinv: set the inverse
##      getinv: get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y){
    ## use <<- to change variables in a differnt environment 
    ## using lexical scoping
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {inv_matrix <<- inverse}
  getinv <- function() {inv_matrix}
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## Function to calculate/retrieve the inverse of matrix
## @x: output of makecacheMatrix list
## return:  inverse of the matrix and set makechaeMatrix list
## this make sure the inverse of matrix is only calculated once 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    ## retrieve it from the list if it is calcuated (not NULL)
    message("getting cached data")
    return(inv)
  }
  ## if it is NULL, calculate it the first time
  data <- x$get()
  inv_matrix <- solve(data, ...)
  ## set the inverse to the makecacheMatrix list structure
  x$setinv(inv_matrix)
  return(inv_matrix)
}

## Test function to evaluate the effect of using cache
test <- function(){
  set.seed(100) # make sure the matrix is repeatable
  mat <- rnorm(1e6) # get the normal distributed random vector with 1e6 elements
  temp <- matrix(mat, nrow = 1e3, ncol = 1e3)
  cachetemp <- makecacheMatrix(temp) # generate a square matrix
  
  start_time <- Sys.time() #register the current system time
  cacheSolve(cachetemp) #solve the inverse first time (no cache available)
  dur <- Sys.time() - start_time # estimate the time difference
  print(dur) # print the duration of calculation time
  
  start_time <- Sys.time() # register time again for the 2nd time
  cacheSolve(cachetemp) #2nd time, retrieve the inverse instead of cacluation
  dur <- Sys.time() - start_time # estimate the time difference
  print(dur) # print the duration of retrieve time
}

test() # run the test