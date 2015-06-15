## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Create the matrix object that contains the function getters and setters
makeCacheMatrix <- function(x = matrix()) {
    #clear the existing inverse 
    i <- NULL
    
    #clear the existing data and inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    #set inverse value
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
#see if there exists a variable in the environments for i, and if not, solve the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i<-x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    #retrieve the matrix 
    data <- x$get() 
    #solve the matrix using built-in function
    i <- solve(data, ...)
    x$setInverse(i)
    i
}