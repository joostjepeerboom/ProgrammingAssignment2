## The function makeCacheMatrix creates a matrix which contains a list of the value of the matrix and the inverse of the matrix
## The function casheSolve calculates the inverse of the special matrix, while first checking if the inverse has already been calculated


makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}

	## Computing the inverse of a square matrix can be done with the solve function in R
	get <- function() x
	setmatrix<-function(solve) m <<- solve
	getmatrix<-function() m

	list(set=set, get=get,
	   setmatrix=setmatrix,
	   getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()

	## Check if the inverse has been calculated before, if so return M (the inverse)
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## If the inverse hasn't been calculate do this now and return M (the inverse)
	matrix <- x$get()
	m <-solve(matrix, ...)
	x$setmatrix(m)
	m
}
