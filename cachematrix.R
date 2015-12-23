## The pair of functions below calculate and cache the inverse of a matrix.  Calculating
## the inverse of a matrix can be resource intensive; therefore, prior to calculating,
## the function will look to see if the inverse of a matrix was previously calcultated.

##`makeCacheMatrix`: This function creates a special "matrix" object 
## that can cache its inverse.  This matrix object is really a List of functions to 

	##1.  set the value of the matrix
	##2.  get the value of the matrix
	##3.  set the value of the inverse
	##4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

            inv <- NULL				
            set <- function(y) {			## assigs the input matrix to the global envrionment so that it can be passed with the list
                    x <<- y				## assigns the matrix to the global environment y		
                    inv <<- NULL			## sets inv equal to null
		}
            get <- function() x			## function created to return the original matrix
            setinverse <- function(d) inv <<- d	## when called sets the inverse to global envrionment
            getinverse <- function() inv		## when called returns the inverse
            list(set = set, get = get,		## creates the list of functions to pass to cacheSolve
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
            inv <- x$getinverse()				## getinverse will return the inverse if it was stored, 
									## otherwise it would return null.  If the matrix changed then x would be a different name and thus Null would be returned 
            if(!is.null(inv)) {					## checks to see if null was returned. 
                    message("getting cached data")	## prints a message to indicate that the invoice was returned from the cache as as opposed to calculated
                    return(inv)					## returns the cached inverse
            }
            data <- x$get()					## gets the matrix that was previously passed
            inv <- solve(data, ...)				## calculates the inverse of the passed matrix
            x$setinverse(inv)					## calls function from the passed list to set the inverse to the global environment
            inv							## returns the inverse of 'x'			
}