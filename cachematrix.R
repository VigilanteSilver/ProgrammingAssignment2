##These functions make inverse matrix computation less resource spendy

##Creates a 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ##Set the value of the matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        ##Get the value of the matrix
        get <- function() x
        ##Set the inverse of the matrix 
        setinverse <- function(inverse) inv <<- inverse
        ##Get the inverse of the matrix
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

        
}

##Computes inverse of 'matrix' object from makeCacheMatrix
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ##Checks if the inverse has already been computed
        if(!is.null(inv)){
                message("Getting cached data...")
                return(inv)
        }
        ##Use setinverse to set the value of the matrix in the cache
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        
}
