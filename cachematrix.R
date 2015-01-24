## These functions work in tandem with the first creating a CacheMatrix, a special
## matrix that can cache its inverse, and the second function actually 
## providing the solving of the inverse and caching ability.


## Creates a CacheMatrix using a regular matrix as its argument.
## It also contains the funtions to set and get both the matrix and its inverse.

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


## Returns the inverse of a cacheMatrix.  If it has not been cached, 
## it computes it and caches it before returning.

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
