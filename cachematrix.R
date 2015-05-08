## The two functions below are used to create a special object that stores a matrix and cache's its inverse.

## The first function, MakeCacheMatrix, creates a special matrix, which is really a list containing a function to
##  1.  set the matrix
##  2.  get the matrix
##  3.  set the matrix inverse
##  4.  get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Start out with the inverse not calculated
  m <- NULL
  
  ## Sets matrix to be inverted
  set <- function(y) {
    
    
            ## Assign to x in parent environment
            x <<- y
            
            ## Assign to m in parent environment
            m <<- NULL
  }
  
  
  ## Gets matrix to be inverted
  get <- function() x
  
  ## Sets inverse in cache
  setinverse <- function(inverse) m <<- inverse
  
  ## Gets inverse from cache
  getinverse <- function() m
  
  ## Returns a list of the functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function inverts the special "matrix" created with makeCacheMatrix above.
## However, it first checks to see if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the inversion process. Otherwise, it inverts the matrix
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## Check to see if the inverse already exists
  
  if(!is.null(m)) {
    
    ## Lets you know that the inverse came from cache
    message("getting cached data")
    
    ## Returns the inverse from cache and exits the function
    return(m)
    
  }
  
  ## If the function reaches this point, then the inverse didn't exist in cache, so it must be calcualted.
  
  ## Gets the original matrix
  data <- x$get()
  
  ## Calculates its inverse.
  m <- solve(data, ...)
  
  ## Puts the inverse into cache.
  x$setinverse(m)
  
  ## Returns the inverted matrix
  m
  
}

## In order to test these functions, you should first creat an ordinary matrix.
## for example: z<-matrix(c(1,2,3,4),2,2) which will result in
##          [,1] [,2]
##      [1,]  1    3
##      [2,]  2    4

## Then create a special "cache matrix" object, using the matrix you just created as the input to makeCacheMatrix.
## cm <- makeCacheMatrix(z)

## Printing out cm will list all of the functions.

## Finally, cacheSolve can now be used on the special object (cm).
## invMatrix<-cacheSolve(cm)

##The first time it is called, the inverse doesn't exist in the cache, so it creates the inverse and stores it in cache.
## Subsequent calls, will retrieve the already calculated inverse from cache (you'll see the message "getting cached
## data"), thus avoiding the computational overhead of recalculating the inverse when it already exists.

## FYI -
## The inverse of z is:
##          [,1] [,2]
##      [1,] -2   1.5
##      [2,]  1  -0.5
