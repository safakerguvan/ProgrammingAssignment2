## Computing and Caching the Inverse of a Matrix
## Two functions below are going to be used to create a matrix 
## and cache its inverse. I will explain what these functions 
## do separately (see below).


## makeCacheMatrix() function creates a special "matrix", which is really 
## a list containing functions to set the value of the matrix, get the
## value of the matrix, set the value of the inverse, get the value of the
## inverse.

makeCacheMatrix <- function(x = matrix()) {

	myinverse <<- NULL
	
	set <- function(y){
		x <<- y
		myinverse <<- NULL
	}
	
	get <- function() x
	
	setmyinverse <- function(inverse) myinverse <<- inverse
	
	getmyinverse <- function() myinverse
	
	list (set = set, 				##gives the name 'set' to the set() function defined above
		get = get, 				##gives the name 'get' to the get() function defined above
		setinverse = setmyinverse,    ##gives the name 'setinverse' to the setmyinverse() function defined above
		getinverse = getmyinverse)	##gives the name 'getinverse' to the getmyinverse() function defined above

}


## cacheSolve() function takes an input argument of type makeCacheMatrix() 
## and then calculates the inverse of the speacial "matrix" created by
## makeCacheMatrix(). However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matric
## and sets the value of the inverse in the cache via the setmyinverse function.

cacheSolve <- function(x, ...) {
 		
	i <- x$getinverse()

	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	
	mymatrix <- x$get()

	i <- solve(mymatrix,...)
	x$setinverse(i)
	i 						## i is now a matrix which is the inverse of 'x'
}