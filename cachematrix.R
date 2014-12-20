## Caching the Inverse of a Matrix.
## The two functions below are used concurrently to find the inverse of a matrix.
## NOTE: makeCacheMatrix(x) has to be called prior to cacheSolve(x).

## --------------------------------------------------------------------------------
##This function creates a special "matrix" object
## 1. Initializes a variable 'm' 
##    (to save inverse matrix, i.e. a cached data)
## 2. Provides function set() to set the matrix
## 3. Provides function get() to obtain "raw" matrix (use to find its inverse);
## 3. Provides function setImatrix() to assign computed inverse matrix (of x) to m;
## 4. Provides function getImatrix() to obtain the cached inverse matrix.
## --------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## --------------------------------------------------------------------------------
## 1. It first checks if the inverse matrix has been found; 
##    if yes, returns the result. 
##    If not, the inverse of x is calculated, saved to cached, and returned.
## NOTE: argument x for this function must be cached, 
##       i.e. a list returned from calling makeCacheMatrix(x).
## --------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data!")
                return(m)
        }
        data <- x$get()
        m <- solve(data) ##Get the inverse matrix 
        x$setmatrix(m)
        m
}
