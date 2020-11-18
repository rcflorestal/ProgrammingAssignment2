##----------------------Programming Assignment 2------------------------------##
## creates two functions, one stores the elements of a cached array and the   ##
## other returns the inverse of the array.                                    ## 
##----------------------Programming Assignment 2------------------------------##

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
                i <- NULL
                set <- function(y){
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                set_i <- function(solve) i <<- solve
                get_i <- function() i
                list(get = get , set_i= set_i, get_i = get_i)
}

## Call the makeCacheMatrix() function 
makeCacheMatrix(matrix(c(1, 0, 0, 2, 1, 0, 3, 4, 1), nrow = 3, ncol = 3))

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...){
        i <- x$get_i()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }else{
                data <- x$get()
                i <- solve(data, ...)
                x$set_i(i)
                return(i)
        }
}

## Cal the cacheSolve() fucntion
cacheSolve(cacheMatrix)
