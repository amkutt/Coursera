## These functions will calculate the inverse of a matrix. However, because matrix inversion can be a costly process, these 
## functions will cache the inverse of a matrix instead of continuously computing it. 

## This function will create a special matrix that can be used to cache the inverse. (Note: Dr. Peng's example on the assignment
## description will be used as a reference)

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function will calculate the inverse of the cached matrix above. 

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
