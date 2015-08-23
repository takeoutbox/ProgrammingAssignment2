
# The makeCacheMatrix handles getting and setting the inverse matrix
makeCacheMatrix <- function(x = matrix()){
  
  inver <-NULL        #initializes the variable to store the inverse matrix as null
  
  #creates the set function to set cached valye of X whatever was passed to y and the inver variable to null
  set <- function (y)
  {
    x<<- y #caches x
    inver<<- NULL #caches y as null
  } 
  
  #creates a function to retrieve the value of x. X is not inverted
  get <- function () X
  setinverse <- function(inverse) inver<<- inverse # creates a function that caches the inverse 
  getinverse <- function () inver # creates a function that retrieves the inverse
  list (set=set, get = get, setnverse = setinverse, getinverse = getinverse) #creates a list to store the objects set, get, setinverse and getinverse
}

## cacheSolve returns the inverse of the matrix from makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  
  inver <- x$getinverse() #calls the getinverse and returns the value of of the cached inverse matrix
  if (!is.null(inver)) #checks to see if inver is already cached. If it is cached returns message and variable
  {
    message ("getting cached data")
    return(inver)
  }
  data <- x$get() #returns the uninverted variable
  inver <- solve(data, ...) #solves for the inverted variable
  x$setinverse(inver) #caches the inverted variable
  inver #returns the inverted variable
}
