## and generate the inverse of this matrix 
## and cache the inverse matrix

## pass an invertible square matrix to this function
## this function gets and set the inverse of the matrix
makeCacheMatrix <- function(m = matrix()) {
	im <- NULL 	
    set <- function(y) { 	
    m   <<- y 	
    im  <<- NULL 	
    } 
	get <- function() m 	
    setsolve <- function(solve) im <<- solve(m) %*% (m) 	
    getsolve <- function() im 	
    list(set = set, get = get,  	
		setsolve = setsolve,  	
        getsolve = getsolve)  
}

## return the inverse matrix, if the matrix is not changed
## recall the function will return its inverse cached matrix

cacheSolve <- function(m, ...) {
	## if m is unchanged, return a matrix that is the cached inverse of 'm'
	im <- m$getsolve() 	
    if(!is.null(im)) { 	
        message("returning your cached matrix") 	
        return(im)   	
    } 
	## Return a matrix that is the inverse of 'm'
	message("your inverse matrix")
	data <- m$get()  	
	im <- solve(data) %*% (data) 
	m$setsolve(im) 	
	im
}
