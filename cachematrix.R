# The following functions stores the matrix into cache, calculates the 
# inverse of the "matrix" and stores the computed result into cache.
# However, cacheSolve call will first checks to see if the inverse
# has already been calculated.If so, it gets the inverse of matrix 
# from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the value of
# the inverse in the cache via the setInverse function.
# it also check if the input matrix is a square matrix
# before computing the inverse
#
# how to run - usecase 1 :
# step 1: mat <- matrix(1:4, 2)
# step 2: cmat<-makeCacheMatrix(mat)
# step 3: cacheSolve(cmat)
# step 4: cacheSolve(cmat) -
# When second time, cacheSolve is called in step 4,matrix inverse returned is a
# cached matrix , thus optimizing the time-comsumption for computation
#
# how to run - usecase 2 :
# step 1: mat <- matrix(1:6, 2)
# step 2: cmat<-makeCacheMatrix(mat)
# step 3: cacheSolve(cmat)
# Since the matrix is not a perfect square matrix ,error message is returned to user
# that says - 'The input matrix is not a perfect square matrix,
# Please input a Perfect square matrix'

# makeCacheMatrix function creates a "matrix"
# set the value of the vector
# get the value of the vector
# setInverse the value of the inverse of Matrix
# getInverse the value of the inverse of Matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
  get <- function() x
  setInverse  <- function(iVal) m <<- iVal
  getInverse <- function() m
  list(set = set,get = get ,
  setInverse = setInverse,
  getInverse = getInverse)
}


# cacheSolve computes the inverse of a matrix
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
    if(!is.null(m)) {
        message("Info:getting matrix from cache")
        return(m)
    }
    data <- x$get()
 
    # condition - if the matrix is  a perfect square matrix
    # then inverse is calulated, if the matrix is not a
    # perfect square , alert message is shown
 
    rowCount<- nrow(data)      
    colCount<- ncol(data)
    if(rowCount== colCount){   
    m <- solve(data)
    x$setInverse(m)
    }
    else{
      stop("The input matrix is not a perfect square matrix, Please input a Perfect square matrix")
    }
    m
}
