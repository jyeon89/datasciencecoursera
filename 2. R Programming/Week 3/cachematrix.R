## Put comments here that give an overall 
## description of what your functions do

## Write a short comment describing this function
## 'x' is a square invertible matrix.
## It sets and gets the matrix. 
## Then it sets and gets the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      
      list(set = set, get = get, 
           setinv = setinv, 
           getinv = getinv)
}


## Write a short comment describing this function
## 'x' is the output of makeCacheMatrix().
## It does the actual calculation 
## to get the inverse of the original matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      len <- length(data)
      sqlen <- sqrt(len)
      mat <- matrix(data, nrow= sqlen, ncol= sqlen)
      inv <- solve(mat)
      x$setinv(inv)
      inv
}

test = function(m){
      ## @mat: an invertible matrix
      
      temp = makeCacheMatrix(m)
      
      start.time = Sys.time()
      cacheSolve(temp)
      dur = Sys.time() - start.time
      print(dur)
      
      start.time = Sys.time()
      cacheSolve(temp)
      dur = Sys.time() - start.time
      print(dur)
}