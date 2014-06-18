##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
###cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.


##Note: makeCacheMatrix takes the input of a matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get returns the object x when called
  get <- function() x
  ###setInverse allows user specification of the inverse of the matrix
  setInverse <- function(invMat) m <<- invMat
  ##getInverse returns the value specified in setInverse
  getInverse <- function() m
  ###makeCacheMatrix stores the list here that contains set,get,setInverse,and getInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

###cacheSolve solves for the matrix inverse of a square invertable matrix. If the matrix
### has been stored using makeCacheMatrix, it will return "getting cached data" and then the inverse
### using getInverse function

### If x has not previously been cached, it is then cached and the value of the inverse is computed
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}