## This functions expand classic matrix function, adding possibility to lookup cache for return of matrix. 

## Seting default matrix, binding "result" variable, making list of operating functions.    

makeCacheMatrix <- function(x <- matrix()) {

  # Empty value 
        result <- NULL
        set <- function(y)  {
  # <<- helps to assign value for variable, "from outside", from another enviroment. 
          x <<- y
          result <<- NULL
        }
  #making list of operating functions
        get <- function() x
        setresult <- function(inverse) result <<- inverse
        getresult <- function() result
        list(set = set, get = get, setresult = setresult, getresult = getresult)

}


## This function has 3 parts: 1. Taking output form previous function. 2. Making check if result already in cache. 3. If it is not - performing calculations. 

cacheSolve <- function(x, ...) {
        
#link to previous function
       result <- x$getresult() result
  
  #chekcing cache for results
       if (!is.null(result)) {
          message("......readaing cache......getting data from cache.......")
          return(result)
  }
  #if result = NULL, run calculations
      matxdata <- x$get()
  #now we should use our key function
      result <- solve(matxdata, ...)
  
  #that`s all, now lets drop result to cache
      x$setresult(result)
  
  #print
  return(result)

}
