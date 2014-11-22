## The following 2 functions allow a user to solve the inverse of a matrix m
## and also cache that value for future use
## Invoke by:
## > mat<-makeCacheMatrix(x)
## > cacheSolve(mat)

## This function makeCacheMatrix takes in a formal argument of type matrix
## The function returns a list of 3 functions
## The 1st function get.matrix simply returns the value of the formal argument
## The 2nd function set.inv.matrix stores the value of the argument passed to it
## into the inv.matrix variable. However inv.matrix is not created locally in 
## set.inv.matrix. It is pointing to the inv.matrix created by makeCacheMatrix
## The 3rd function get.inv.matrix returns the variable inv.matrix

makeCacheMatrix <- function(x = matrix()) {

  inv.matrix<-NULL #initialize to NULL
  
  get.matrix <- function() x  #returns matrix x
  set.inv.matrix <- function(im) inv.matrix <<- im #set the variable inv.matrix
  get.inv.matrix <- function() inv.matrix  #returns inv.matrix
  list(get.matrix = get.matrix, set.inv.matrix = set.inv.matrix, get.inv.matrix = get.inv.matrix)
  
}


## This function cacheSolve takes in a formal argument x and other in ...
## It returns a matrix that is the inverse of x
## The function expects a special matrix in the argument
## It checks if the inverse of the matrix was cache and returns the cache value if so
## If not it will solve the inverse, cache the value and also returns the value

cacheSolve <- function(x, ...) {
  m<-x$get.inv.matrix()              ##get inverse matrix
  if(!is.null(m)){                   ##if exists, just return cache value
    print("Get from cache")
    return(m)
  }
  m<-x$get.matrix()                  ##otherwise get the matrix
  m<-solve(m)                        ##inverse it
  x$set.inv.matrix(m)                ##cache it
  return(m)                          ##returns it
}
