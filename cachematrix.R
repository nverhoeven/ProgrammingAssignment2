## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# mimicing the example one here
makeCacheMatrix <- function(x = matrix()) {
#inv will store the cached inverse matrix
        inv <- NULL 
        #set matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get matrix
        get <- function() x
        #set inverse
        setinverse <- function(inverse) inv <<- inverse
        #get inverse
        getinverse <- function() inv
        # return 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## return cached inverse or calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        #calculation if needed
        data <- x$get()
        inv <- solve(data)
        #set inverse
        x$setinverse(inv)
        #return
        inv
}
