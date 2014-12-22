## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
## set the value of the matrix
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
## get the value of the matrix
                get <- function() x
## set the value of the inverse
                setmatrix<- function(solve) m <<- solve
## get the value of the inverse
                getmatrix <- function() m
                list(set = set, get = get,
                     setmatrix = setmatrix,
                     getmatrix = getmatrix)
        }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
## Check to see if the inverse has already been calculated        
        m <- x$getmatrix()
## If so, it gets the value from the cache and skips the computation
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## If not, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmatrix function.
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
