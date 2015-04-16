## Series of functions that have been written as Part of Coursera Rprog-013
## The functions cache the inverse of a matrix using the <<- operator
## that can be used to assign a value to an object in an environment that
## is different from the current environment.

## An example usage is included below:
##
## > source("cachematrix.R")
## > p <- makeCacheMatrix( matrix(c(1,2,10,11), c(2,2)))
## > cacheSolve(p)
## >          [,1]       [,2]
## [1,] -1.2222222  1.1111111
## [2,]  0.2222222 -0.1111111
## > cacheSolve(p)
## getting cached data
##            [,1]       [,2]
## [1,] -1.2222222  1.1111111
## [2,]  0.2222222 -0.1111111
##
##
##
## As per the instructions it has been assumed that the matrix is invertable 
## and no input testing has beenincluded

## makeCacheMatrix function creates a special "matrix", that is a list
## containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # cache holds the cached value or NULL if not
        # set initial Value to NULL
        cache <- NULL
        
        #stores a matrix
        set <- function(y) {
                x <<- y
                cache <<- NULL  # new matrix has been stored so clear cache
        }
        
        # returns the stored matrix
        get <- function() {
                x
        }
        
        # cache the inverse
        setinverse <- function(inv){
                cache <<- inv
        }
        
        # get the cached value
        getinverse <- function(){ 
                cache
        }
        
        list(
                set = set, 
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
        

}


## cacheSolve function calcualtes the nverse of a "special" matrix created 
## with the makeChaceMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        
        # if value has been cached - get it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # otherwise get the matrix, calculate the inverse and store it
        # in the cache
        
        data <- x$get()                 # get the matrix
        inverse <- solve(data, ...)     # calculate inverse
        x$setinverse(inverse)           # store the inverse in the cache
        
        inverse                         # return the value                         

}
