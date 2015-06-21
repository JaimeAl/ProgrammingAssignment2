
## This function defines a set of functions and lists them so
## they can be called, and be all in the same enviroment.
## To call this fucntions you would need to assign the
## result of makeCacheMatrix() to something so the list is
## available in the global environment.

makeCacheMatrix <- function(Working_Matrix = matrix()) {
        
	## Set a defalt value for the inverse in case it hasn't
	## been calculated yet.       

	Inverse <- NULL 
        
	## Define the functions. The names of variables makes it
	## pretty much self explanatory. If matrix is changed
	## Inverse is set to NULL in the parent environment.

        setmatrix <- function (New_Matrix) {
                Working_Matrix <<- New_Matrix
                Inverse <<- NULL }

        getmatrix <- function() Working_Matrix
        setinverse <- function(New_Inverse) Inverse <<- New_Inverse
        getinverse <- function() Inverse
        
	## And now we just list the functions.        

	list(setmatrix = setmatrix, getmatrix = getmatrix, 
        setinverse = setinverse, getinverse = getinverse)       
}


## This function checks if the matrix' inverse has already
## been calculated, and calculates it if it hasn't. It calls the
## the functions from the list returned by MakeCacheMatrix. Takes
## this list as an argument.


cacheSolve <- function (Function_List, ...){
        
        Inverse <- Function_List$getinverse()
        
        if(!is.null(Inverse)) {
                message("Getting cached data")
                return(Inverse) 
        }
        Working_Matrix <- Function_List$getmatrix()
        Inverse <- solve(Working_Matrix)
        Function_List$setinverse(Inverse)
        Inverse
}