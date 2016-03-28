##creates a matrix and a place to store the inverse of that matrix. Call normal matrix via x$get() and inverse (after running cache solve) as x$getinverse()
makeCacheMatrix <- function(x = matrix()) { 
  
   m <- NULL
  
  set <- function(y) { ##sets new table y as the normal table and turns the global inverse table to null
    x <<- y 
    m <<- NULL 
  }
  
  get <- function() { ## returns normal table
    x  
  } 
  
  setinverse <- function(solve) { ## sets inverse table after running cacheSolve
    m <<- solve  
  } 
    
    
  getinverse <- function() { ## returns inverse table after running cacheSolve
    m  
  } 
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

##cacheSolve creates a chached version of the x$getmatrix()'s inverse
cacheSolve <- function(x, ...) {
        
  m <- x$getinverse() ##stores the value of x$getinverse() as global variable m in prep for null check
  
  if(!is.null(m)) { ##returns the cached value if m is not null
    message("getting cached data")
    return(m)
  }
  
  data <- x$get() ##stores x get as data

  m <- solve(data) ## store inverse of the data table in global variable m
  
  x$setinverse(m) #sets and stores the value of global variable m   
    
}
  
  
 

