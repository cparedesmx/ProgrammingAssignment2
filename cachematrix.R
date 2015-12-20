## These functions implement a matrix object where the inverse is solved and
## cached. It is assumed that the matrix is always invertible.
## Written while taking the R Programming course on Coursera.


## makeCacheMatrix takes a matrix and creates an object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse matrix to NULL
    inv <- NULL
    
    ## Allow for replacing the matrix and automatically set the inverse to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Return the matrix we are working with
    get <- function() x
    
    ## Allow the user to manually set the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    
    ## Return the cached inverse of the matrix
    getinverse <- function() inv
    
    ## Define (return) the functions that are public
    list(set = set, get =get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix if it already exists, otherwise
## it computes the inverse using solve. 

cacheSolve <- function(x, ...) {
    ## Ask the matrix for its inverse
    inv <- x$getinverse()
    
    ## If the inverse is not NULL, notify that we are using a cached inverse
    ## and return it (exit the function)
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    ## If the inverse is NULL, get the original matrix and solve it.
    matrix <- x$get()
    inv <- solve(matrix, ...)
    
    ## Store the inverse of the matrix
    x$setinverse(inv)
    
    ## Return the inverse of the matrix
    inv
}