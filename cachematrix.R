## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Here is where we will set, get the matrix and then set and get the inverse
##We first need to assign x function as a matrix
makeCacheMatrix <- function(x = matrix()) {  
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL  
    }
##We then need to get the function and set it to then get the inverse of that    
get <- function() x
setinv <- function(inverse) m <<- inverse
getinv <- function() m
list(set = set, get = get,
     setmean = setmean,
     getmean = getmean)

}


## Write a short comment describing this function
##Here we are then assigning cacheSolve for all the return of the makecachematrix assignment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) { ##if it is found then it will stop and give us this message.  
    
    message("Getting Cached Data")
    return(m)  
  }  ##if answer is new then it will continue and will return the inverse
  data <- x$get()  ## the answer from the makecashematrix function
  m <- solve (data, ...)  ##calculating 
  x$setinv (m)## here it sets it 
  return(m) ## display the inverse
}
