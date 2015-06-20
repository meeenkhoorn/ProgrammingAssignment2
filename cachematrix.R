## The functions makeCacheMatrix and cacheSolve operate together.
## It can take a matrix and can return its inverse.
##
## It will check whether the inverse matrix already exists in the cache memory of the computer. 
## If not, then it calculates the inverse matrix. Otherwise it returns the cached matrix.
## The software assumes a square invertable matrix (in concordance with the assingment description).
##
## Example:
## sample <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)))
## sample$get()         returns the matrix
## sample$getinverse()  returns the inverse matrix if already calculated, otherwise it returns NULL
## cacheSolve(sample)   set the inverse matrix of sample if it was NULL (and returns it)
## cacheSolve(sample)   returns the cached inverse matrix (if it was not NULL)


## makeCacheMatrix holds two object variables: 
##  - the original matrix
##  - the inverse matrix (default value is NULL)
## makeCacheMatrix holds four class methods:
##  - get() : returns the original original matrix
##  - set(y) : set the matrix to y
##  - getinverse() : returns the inverted matrix
##  - setinverse(z) : set the inverse matrix to z


makeCacheMatrix <- function(x = matrix()){
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of matrix 'x'. The inversion is done with the standard R solve(x) command.
## cacheSolve needs an input object of the type makeCacheMatrix. That object can allready have an non-NULL inverse matrix,
## then it read and returns that matrix. Otherwise it calculates the inverse with the solve command. The inverse matrix 
## is then assigned to the object inverse matrix variable. 

cacheSolve <- function(x, ...){
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached matrix")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}