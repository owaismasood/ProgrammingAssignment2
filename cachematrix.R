## These function implemnetation allow the user to Cache Matrix inverse and use
## them for subsequent use. 

## makeCacheMatrix : This is an Class Based implementation of cacheMatrix. 
##                    Takes A Matrix input. And Returns a CacheMatrix list
##                    containing getter and setter functions
## Avaialable functions
#  - get() - Return the martrix
#  - getInverse() - Returns the inverse of Input matrix
#  - set(x) - Assign the matrix
#  - setInverse(x) - Assign the 

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
    
  get <- function() x                    #Getter for data matrix
  
  getinverse <- function() inverse       #Getter for the inverse of the data matrix
  
  
                                        #Setter for the inverse matrix.
  setinverse <- function(setinverse) inverse <<- setinverse
  
                                         #Setter for data matrix
  set <- function(y) {                  
    x <<- y
    inverse <<- NULL
  }
  
  list(set = set, get = get,            ## Return object
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve :       This function represents the implementation of makeClassObjects . 
##                    It takes Ta makeClassObject list as an input . And Returns a Cached Matrix inverse                     


cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()         
  if(!is.null(inverse)) {                      ## Returning cached Inverse if already calculated
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()                            ## Calculating a new inverse for new data.
  inverse <- solve(data, ...)                ## and Storing it in the cache and returning it
  x$setinverse(inverse)
  return(inverse)                           
}
        ## Return a matrix that is the inverse of 'x'

