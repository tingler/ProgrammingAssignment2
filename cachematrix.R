##############################################################
# Coursera Data Science course "R Programming" assignment #2 #
#               Github account name "tingler"                #
##############################################################

####################################################################################
# These functions operate on a special "matrix" object that can cache its inverse. #
####################################################################################

# This function creates the special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {

	# start with no inverse
        inv <- NULL

	# create a function to set the matrix from a new value
        set <- function(new) {
                x <<- new
                inv <<- NULL	# there is no inverse for this one yet
        }

	# create a function to get the matrix
        get <- function() x

	# create a function to set the matrix inverse
        setinv <- function(newinv) inv <<- newinv

	# create a function to get the matrix inverse
        getinv <- function() inv

	# return the object as a list containing the 4 functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

####################################################################################

# This function returns a previously cached inverse of a "CacheMatrix".
# If the inverse has not already been calculated, a new one is computed and cached.

cacheSolve <- function(x, ...) {

        # get the previously cached inverse
        inv <- x$getinv()

	# if nothing was cached, compute and cache it now
        if (is.null(inv)) {
		inv <- solve(x$get(), ...)
		x$setinv(inv)
        }

	# finally return it
        inv
}
