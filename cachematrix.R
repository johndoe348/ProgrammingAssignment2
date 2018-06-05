## Function that takes a matrix as an argument and caches 
## the matrix until the function is called again
makeCacheMatrix <- function(matrixInput = matrix()) {
        inverse <- NULL
        set <- function(newMatrix){
                matrixInput <<- newMatrix
                inverse <<- NULL
        }
        get <- function() matrixInput
        setMatrix <- function(inv) inverse <<- inv
        getMatrix <- function() inverse
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
        
}


## Write a short comment describing this function
## Function that takes a list of functions as an argument
## and checks for cached data, if ther is no cached data
## then it calculates the inverse of the matrix and passes the result
## back to makeCacheMatrix for it to be cached
cacheSolve <- function(funList) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- funList$getMatrix()
        if(!is.null(inverse)){
                print("This is cached inverse matrix")
                return(inverse)
        }
        data <- funList$get()
        inverse <- solve(data)
        funList$setMatrix(inverse)
        inverse
}