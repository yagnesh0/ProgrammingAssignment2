## makeCacheMatrix() initializes empty matrix 'x'
## set() gives values and dimensions to matrix 'x' and caches
## get() prints out matrix 'x'
## setInverse() stores inverted matrix 'm' in cache
## getInverse() prints out inverted matrix 'm'

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ## set() gives values and dimensions to matrix 'x' and caches
        x <<- y
        m <<- NULL
    }
    get <- function() x ## get() prints out matrix 'x'
    setInverse <- function(inverse) m <<- inverse ## setInverse() stores inverted matrix 'm' in cache
    getInverse <- function() m ## getInverse() prints out inverted matrix 'm'
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() will solve for the inverse of matrix 'x'
## before computing it, it will check if there is already a cached version of it
## it will print that cached version if found, else will compute

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m) #ends function if cached found
    }
    data <- x$get() #else solves for the inverse
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

