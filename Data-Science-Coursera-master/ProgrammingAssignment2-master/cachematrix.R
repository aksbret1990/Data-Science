## Both the functions together help in getting the inverse of the matrix irrespective whether the inverse is calculated or not already. 

## makeCacheMatrix function is responsible for creating special object that can cache its inverse
## makeCacheMatrix creates a list containing a function to
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(data_matrix = matrix()) {
        stored_inverse <- matrix()
        
        set_matrix <- function(y) {
                data_matrix <<- y
                stored_inverse <<- matrix()
        }
        
        get_matrix <- function() {
                return (data_matrix)
        }
        
        set_inverse <- function(sent_replacement_inverse){
                stored_inverse <<- sent_replacement_inverse
        }
        
        get_inverse <- function() {
                return(stored_inverse)
        }
        
        
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## The cacheSolve function calculates the inverse of the matrix. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache via the set_inverse function.

cacheSolve <- function(made_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        local_inverse <- made_matrix$get_inverse()
        
        if(!all(is.na(local_inverse))) {
                message("getting cached data")
                return(local_inverse)
        }
        else {
                local_matrix <- made_matrix$get_matrix()
                local_inverse <- solve(local_matrix)
                
                made_matrix$set_inverse(local_inverse)
                return(local_inverse) 
        }
}
