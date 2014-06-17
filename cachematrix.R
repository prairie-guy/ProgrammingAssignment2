## cachematrix.R

## These two functions demonstrate the use of a cache to avoid potentially costly duplicative calls on functions
## that have been previously executed. Specifically, this code demonstrates cacheing in the context of the inverstion
## of a matrix.


## Initially, 'makeCacheMatrix' takes a valid matrix as its input and returns an instance of a 'CacheMatrix',
## a list comprised of four functions: set, get,set_i_mat and get_i_mat. Importantly, this instance of
## 'CacheMatrix' has its own environment in which specific values of 'mat' and 'i_mat' can be defined and saved.
## Because the four functions in the list are defined within this environment, subsequent calls of set, get,
## set_i_mat and get_i_mat can reference these pecific values of 'mat' 'i_mat'.

makeCacheMatrix <- function(mat = matrix()) {
 i_mat <- NULL
    
 list(set = function(m) {mat <<- m; i_mat <<- NULL},
      get = function() mat,
      set_i_mat = function(im) i_mat<<- im,
      get_i_mat = function() i_mat)
} 



## The function 'cacheSolve' accepts an instance of 'CacheMatrix'. It first takes and executes the 'get' function.
## If this is the first time 'cacheSolve' is called on this instance of 'CacheMatrix', it calculates the inverse
## of the matrix and uses 'set_i_mat' to save the inverse of the matrix within the environment of the instance of
## 'CacheMatrix'. If this is not the first time this instance of 'CacheMatrix' has been called by 'cacheSolve',
## then the previously calculated invserse of the matrix is returned.

cacheSolve <- function(mat, ...) {
    ## Return a matrix that is the inverse of 'mat'
    i_mat <- mat$get_i_mat()
    
    if (!is.null(i_mat)) {
        message("getting cached data")
        return(i_mat)
    }
    else {
        i_mat <- solve(mat$get(), ...)
        mat$set_i_mat(i_mat)
        return(i_mat)
    }
}

## Test code to make sure everyting works

test_matrix_cache <- function() {
    m <- matrix(sample(1:20,9), 3,3)
    print("m-matrix")
    print(m)
    print("inv of m-matrix")
    print(solve(m))
    m1 <- makeCacheMatrix(m)
    print("m1 <- makeCacheMatrix(m)")
    print("initial: cacheSolve(m1)")
    print(cacheSolve(m1))
    print("subsequent: cacheSolve(m1)")
    print(cacheSolve(m1))     
    # Need to test set
}


## Sample results of test_matrix_cache. Everything works as expected.

## test_matrix_cache()
## [1] "m-matrix"
##      [,1] [,2] [,3]
## [1,]   13   17   18
## [2,]    5   19   15
## [3,]   20    2    8
## [1] "inv of m-matrix"
##            [,1]       [,2]       [,3]
## [1,] -0.1865443  0.1529052  0.1330275
## [2,] -0.3975535  0.3914373  0.1605505
## [3,]  0.5657492 -0.4801223 -0.2477064
## [1] "m1 <- makeCacheMatrix(m)"
## [1] "initial: cacheSolve(m1)"
##            [,1]       [,2]       [,3]
## [1,] -0.1865443  0.1529052  0.1330275
## [2,] -0.3975535  0.3914373  0.1605505
## [3,]  0.5657492 -0.4801223 -0.2477064
## [1] "subsequent: cacheSolve(m1)"
## getting cached data
##            [,1]       [,2]       [,3]
## [1,] -0.1865443  0.1529052  0.1330275
## [2,] -0.3975535  0.3914373  0.1605505
## [3,]  0.5657492 -0.4801223 -0.2477064
