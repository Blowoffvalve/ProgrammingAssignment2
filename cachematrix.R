## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse. It features the ability to store and retrieve the inverse
## of a matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	setsolve<- function(solve) m<<- solve
	getsolve<- function()m
	list(set= set, get = get, setsolve = setsolve, getsolve=getsolve)
}
## It uses the getsolve method to attempt to pull the value of solve from the matrix above. If it can't find(see ##CheckingExistenceMatrix)
## It calculates it(see ##CalculateSolveValue) and stores it using the setsolve method( see ##StoreInverse)
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
	if(!is.null(m)){        ##CheckingExistenceMatrix
		message("getting solve cached from an earler computation")
		return(m)
	}
	data<- x$get()
	m<- solve(data,...) ##CalculateSolveValue
	x$setsolve(m) ##StoreInverse
	m
}
