#### Put comments here that give an overall description of what your
#### functions do

# Here we creates an object that has its own member functions
# to get and set a matrix and its inverse. Those objects are accessed
# in the cacheSolve function which calculates and stores the inverse
# of a matrix

#### Write a short comment describing this function

# This function creates a special matrix object that stores both a matrix
# and its inverse and provides functions to access those matrices
# and also to save those matrices. 
# Description about the member functions:
# 1. set(y) function sets the matrix to an object
# 2. get() functions returns the matrix
# 3. setInverse(invMatrix) function sets the inverse of the matrix to an object
# 4. getInverse() function returns the inverse of the matrix that is stored
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(invMatrix) inv <<- invMatrix
        getInverse <- function() inv
        # Here we return the list containing all the member functions
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


#### Write a short comment describing this function

# This function calculates the inverse of the special matrix with the 
# above function. It first checks whether the inverse is already 
# calculated, if it is then it will return the cached inverse matrix
# else it will calculate the inverse and then store it in the cache and
# then return the inverse to the calling function
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        normalMatrix <- x$get()
        inv <- solve(normalMatrix)
        x$setInverse(inv)
        inv
}
