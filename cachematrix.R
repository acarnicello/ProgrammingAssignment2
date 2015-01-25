## These functions save the inverse of a matrix outside of the fuctions so that
## it may be called if already calculated instead of being calulated each time. 



## Takes a matrix as its argument. It contains funtions to cache and retrive 
## the matrix and the inverse.

makeCacheMatrix <- function(x = matrix())
{
    inv<- NULL
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Returns the inverse of a cached Matrix.  If the inverse has not been cached, 
## it computes the inverse and uses the setinv function from makeCacheMatrix
## to cache it before returning.

cacheSolve <- function(x, ...) 
{
        inv <- x$getinv()
        
        if(!is.null(inv))
        {
            message("getting cached data")
            return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        
        inv        ## Return a matrix that is the inverse of 'x'
}
