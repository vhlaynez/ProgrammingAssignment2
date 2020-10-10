makeCacheMatrix <- function(x = matrix()) {
    ## inverse property
    i <- NULL
    
    set_matrix <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    get_matrix <- function() {
        ## Return the matrix
        m
    }
    
    set_inverse <- function(inverse) {
        i <<- inverse
    }
    
    get_inverse <- function() {
        i
    }
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}

cacheSolve <- function(x, ...) {
 
    inverse_matrix <- x$get_inverse()
    
    if( !is.null(inverse_matrix) ) {
        message("getting cached data")
        return(inverse_matrix)
    }
    
    matrix <- x$get_matrix()
 
    inverse_matrix <- solve(matrix) %*% matrix
    
    x$set_inverse(inverse_matrix)
    
    inverse_matrix
}
