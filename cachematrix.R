# The first function, makeCacheMatrix creates a special "matrix", which has row name "functionCall", and column name of function name
#there are 4 colulmns each stores below function
# set the value of the matrix
# get the value of the matrix
# set the value of the Inverse
# get the value of the Inverse

makeCacheMatrix<- function(x = matrix())
  
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  rmatrix<-matrix(c(set,get,setinverse,getinverse),1,4)
  colnames(rmatrix) <- c("set","get","setinverse","getinverse")
  rownames(rmatrix) <- c("functionCall")
  
  
  rmatrix 
  
}

#The following function calculates the inverse of the special "matrix" created with the above function.
#However, it first checks to see if the inverse has already been calculated. If so, it gets the mean from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) 
  
{
  
  m <-  x[["functionCall","getinverse"]]()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x[["functionCall","get"]]()
  
  m <- solve(data)
  x[["functionCall","setinverse"]](m)
  
  m
}