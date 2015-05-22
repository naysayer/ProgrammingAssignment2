## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
# very much like the example, we will be setting the matrix, getting the matrix
# setting the inverse of the matrix, and gettin the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        setMatrix <- function(matrix){
                x <<- matrix
                inverse <<- NULL
        }
        getMatrix <- function() x
        
        setInverseMatrix <- function(solved_matrix) inverse <<- solved_matrix
        
        getInverseMatrix <- function() inverse
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getInverseMatrix())){
                # the inverse matrix is defined, so return it 
                x$getInverseMatrix()
        }else{
                # the inverse matrix is not defined, set and return it 
                inverse <- solve(...)
                x$setInverseMatrix(inverse)
        }
        inverseMatrix <- x$getInverseMatrix()
        inverseMatrix
}
