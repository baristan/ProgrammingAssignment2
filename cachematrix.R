## Put comments here that give an overall description of what your
## functions do


##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <<- NULL
	set <- function(x) {
                 v <<-x
		 m <<- NULL
        }	
	setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
	get <- function() v
	list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
	
}


## This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
         if(!is.null(m)) {
                 message("getting cached data")	##it gets the inverse from the cache and skips the computation
                 return(m)
         }
         data <- x$get()	
         m <- solve(data, ...)	##it calculates the inverse of the data and sets the value of the inverse 
         x$setinverse(m)	##in the cache via the setinverse function
         m
         
}
