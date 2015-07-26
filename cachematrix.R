## A pair of functions:
##   makeCacheMatrix - creates an object that represents a square invertible
##                     matrix, including a slot for a cached value of its
##                     inversion;
##
##   cacheSolve      - operates on an object returned by makeCacheMatrix, and
##                     inverts the matrix it represents (using the object's
##                     cached solution if it has one; calculating the
##                     solution and storing it in the cache if not).

# Create an abstract object representing a matrix.
#
# Argument:
#   internalMatrix    - optional. An R matrix to use as internal matrix. Empty
#                       matrix if not specified.
# Return value: an abstract object containing the following "methods":
#   $set(matrix)      - sets the internal R matrix
#   $get              - returns the internal R matrix
#   $setCache(matrix) - sets the cached solution value to matrix
#   $getCache         - returns the cached solution value (NULL if it
#                       hasn't been set).
#
# All operations on the object assume that the internal matrix is square and
# invertible.
makeCacheMatrix <- function(internalMatrix = matrix()) {
  # initialize cached solution to NULL, which signals to cacheSolve that the
  # solution hasn't been computed yet
  cachedSolution <- NULL
  
  # a function which sets this object's internal matrix to value passed in
  set <- function(matrix) internalMatrix <<- matrix

  # a function which gets this object's internal matrix
  get <- function() internalMatrix
  
  # a function which sets this object's cached solution
  # NOTE: this isn't used in the current exercise
  setCache <- function(matrix) cachedSolution <<- matrix
  
  # a function which gets this object's cached solution
  getCache <- function() cachedSolution
  
  # assemble all the "methods" into an object and return it
  list(set = set,
       get = get,
       setCache = setCache,
       getCache = getCache)
}

# Solve an object of the kind returned by makeCacheMatrix, by taking its
# inverse.
#
# Argument:
#   matrixObject - an object of the kind returned by makeCacheMatrix,
#                  representing a matrix (assumed to be square, invertible) and
#                  having the "methods" $set(matrix), $get(),
#                  $setCache(matrix), and $getcache().
# Return value: the inverse of the matrix represented by matrixObject.
cacheSolve <- function(matrixObject) {
  # get cached solution, if there is one yet
  solution <- matrixObject$getCache()
  
  # if there is a solution, notify user that output is cached, not computed.
  if(!is.null(solution)) {
    message("getting cached data")
  } else {
    # otherwise, calculate it
    # start by getting internal R matrix
    internalMatrix <- matrixObject$get()
    # calculate inverse
    solution <- solve(internalMatrix)
    # cache it in matrixObject for later use
    matrixObject$setCache(solution)
  }

  # return solution, whether it was cached or calculated here
  solution
}