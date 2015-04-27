makeCacheMatrix <- function(x = matrix()) {
       
        invMatrix = NULL
        set = function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get = function() x
        setinvMatrix = function(inverse) invMatrix <<- inverse 
        getinvMatrix = function() invMatrix
        list(set=set, get=get, setinvMatrix=setinvMatrix, getinvMatrix=getinvMatrix)
}

cacheSolve <- function(x, ...) {
        
        invMatrix = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(invMatrix)){
                # get it from the cache and skips the computation. 
                message("getting matrix from cache")
                return(invMatrix)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        invMatrix = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinvMatrix(invMatrix)
        
        return(invMatrix)
}
