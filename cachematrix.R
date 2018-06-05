## Write a short comment describing this function
## Function that takes a matrix as an argument and caches 
## the matrix until the function is called again
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(matrix) m <<- matrix
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
        
}


## Write a short comment describing this function
## Function that takes a list of functions as an argument
## and checks for cached data, if ther is no cached data
## then it calculates the inverse of the matrix and passes the result
## back to makeCacheMatrix for it to be cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)){
                print("This is cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setMatrix(m)
        m
}
