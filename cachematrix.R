## 07.05.2017: This is homework for Week 3 of R Programming course on coursera.org
## The functions below cache the inverse of a matrix to avoid potentially costly recalculations when not necessary. 
## The code and comments below are based on the sample provided by Prof. Peng as part of the assignment.


## This function creates a matrix object that can cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) 
{
        
        ## Input:
        ##      x:      matrix whose inverse is to be computed and stored
        ##
        ## Output:      list of 4 functions: set, get, setinverse and getinverse that set x equal to the argument passed, 
        ##                      return the stored matrix, set the inverse of the stored matrix and return it, respectively
        ## 

        Inverse <- NULL
        
        set <- function(y) 
        {
                if (sum(x!=y)>0) # to avoid any unnecessary recalculation of the inverse, only if x<>y, then...
                {
                        x <<- y # ... set x to y and...
                        Inverse <<- NULL # ... reset the inverse since it has not been calculated yet for the new x
                }
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
        if(!is.null(Inverse)) # if the inverse has already been calculated, then...
        {
                message("Cached inverse returned.")
                return(Inverse) # ... return the stored inverse
        }
        data <- x$get()
        Inverse <- solve(data, ...) # if the inverse has not been calculated yet, compute...
        x$setinverse(Inverse) # ... store and...
        Inverse # ... return it
        
}
