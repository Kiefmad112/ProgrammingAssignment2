## Cache the inverse of a Matrix using Solve()

## Function creates a list and stores the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## Set inverse to NULL
    inverse <- NULL                           
    
    
    ## Set value of matrix: Set x to y, set m to NULL
    set <- function(y) {                
        x <<- y                          
        inverse <<- NULL                       
    }
    
   
    ## Get value of matrix
    get <- function() {x}             
    

    ## Set inverse value
    setinv <- function(solve) inverse <<- solve
    
    
    ## Get inverse value
    getinv <- function() inverse
    
    
    ## Return the list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    

}


## Function determines if matrix inverse has been cached or not and returns result.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        
        
        ## Determine if inverse has been cached
        if(!is.null(inverse)) {
            
            message("Retrieving inverse data") ## Message if already cached
            return(inverse)
        }
    
        ## If not cached
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
