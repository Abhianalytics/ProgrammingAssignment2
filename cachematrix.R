## These two functions together help do a matrix inverse.
## At the same time, they help in demonstrating the caching capability of R

## Function makeCacheMatrix :  This function creates a special "matrix" object 
## that can cache its inverse.

        
makeCacheMatrix <- function(x = matrix(), nr=numeric(), nc=numeric()) 
{
        ## initialize the value of mat variable to NULL
        mat <- NULL
     
        mat1 <- matrix(x,nr,nc)
        
        ## set function for setting the value 
        set <- function(y)
        {
                mat1 <<- matrix(y,nr,nc)
                mat <<- NULL
                
        }
        
        ## get function to retrieve the value 
        get <- function() mat1
        
        ## set function to set the value of the matrix
        setmat <- function(smat) mat <- matrix(smat,nr,nc)
        
        ## getmat function to retrieve the value of the matrix
        getmat <- function() mat
        
        ## to create the list which is returned by this function
        list (set=set, get=get, setmat=setmat, getmat=getmat)
                
}


## Function cacheSolve :  This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

        cacheSolve <- function(x, nr, nc, ...) 
{
                ## Use getmat() to check if the value is cached 
                mat <- x$getmat()
                
                ##This piece of code checks if the matrix is already cached, then simply retrieve the cached value & return
                if(!is.null(mat)) 
                {
                        message("getting cached data")
                        return(mat)
                }
                data <- x$get()
                
                ## Use the function solve() to return a matrix that is the inverse of 'x'
                mat <- solve(data, ...)
                
                x$setmat(mat)
                
                ## Return the inverse matrix stored in mat 
                mat
                
               
}
