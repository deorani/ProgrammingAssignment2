## To save time required for calculating inverse of a matrix again and
## again, lets just store it along with the matrix. We will make a 
## function that creates a list containing the matrix, its inverse as 
## well as functions to set and get the inverse. We will make another
## function to calculate or obtian the inverse in case when inverse 
## has already been calculated or not, respectively.

## The following function takes a matrix and returns a special "matrix"
## which is just a list containing the matrix itself, its inverse, and 
## functions to set the value of inverse, as well as obtain it.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function finds the inverse of the special matrix
## created above by "makeCacheMatrix" function. It first checks 
## whether the inverse has already been calculated, and returns it. 
## Else it calculates the inverse and returns it as well as stores
## it in the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        
        x$setinv(inv)
        inv
}
