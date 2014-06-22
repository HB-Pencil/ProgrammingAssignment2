## Just like Adam Gruer explained in the thread of "Can we have  
## a demonstration of the mean example", these two functions work
## together to calculate the inverse of a matrix and cache the 
## value. 

## makeCacheMatrix() creates a list of four functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the martix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
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


## cacheSolve() first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from 
## the makeCacheMatrix() function. Otherwise, it calculates
## the inverse and sets the value of the inverse in the cache.

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
