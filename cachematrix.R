## 07.05.2017: This is homework for Week 3 of R Programming course on coursera.org
## The functions below cache the inverse of a matrix to avoid subsequent recalculations when not necessary. 
## The code and comments below are based on the sample provided by Prof. Peng as part of the assignment.


## This function creates an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        
        ## Input:
        ##      x:      a matrix whose inverse is to be computed and stored
        ##
        ## Output:      a list of 4 functions: set, get, setinverse and getinverse that set the matrix equal to x, return 
        ##                      the stored matrix, set the inverse of the stored matrix and return it, respectively
        ## 

        Inverse <- NULL
        
        set <- function(y) 
        {
                x <<- y
                Inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) Inverse <<- inverse
        
        getinverse <- function() Inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## This function computes the inverse of the special matrix object returned by makeCacheMatrix above. If the inverse 
##      has already been calculated (and the matrix has not changed), cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        
        ## Input:
        ##      x:      an matrix object of the type returned by makeCacheMatrix above
        ##      ...:    optional parameters, currently none
        ##
        ## Output:      the inverse of the matrix stored in x
        ## 
        
        Inverse <- x$getinverse()
        if(!is.null(Inverse)) 
        {
                message("Cached inverse returned.")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setinverse(Inverse)
        Inverse
        
}
