context("makeCacheMatrix")

testMatrix1 <- matrix(c(1,0,5,2,1,6,5,4,0), nrow = 3, ncol = 3)
correctSolution1 <- solve(testMatrix1)

testMatrix2 <- matrix(c(5,4,6,3,2,1,7,9,8), nrow = 3, ncol = 3)
correctSolution2 <- solve(testMatrix2)

testMatrix3 <- matrix(c(8, 4, 3, 3, 9, 3, 8, 9, 4), nrow = 3, ncol = 3)
correctSolution3 <- solve(testMatrix3)

describe("result with no argument", {
  it("is a list", {
    result <- makeCacheMatrix()
    expect_that(class(result), equals("list"))
  })
  
  it('has all four "methods"', {
    result <- makeCacheMatrix()
    expect_false(is.null(result$set))
    expect_false(is.null(result$get))
    expect_false(is.null(result$setCache))
    expect_false(is.null(result$getCache))
  })
  
  it("has working $set and $get", {
    result <- makeCacheMatrix()
    expect_identical(result$get(), matrix())
    
    result$set(testMatrix1)
    expect_identical(result$get(), testMatrix1)
  })
  
  it("has working $setCache and $getCache", {
    result <- makeCacheMatrix()
    result$setCache(correctSolution1)
    expect_identical(result$getCache(), correctSolution1)
  })
})

describe("result with an argument", {
  it("has working $set and $get", {
    result <- makeCacheMatrix(testMatrix1)
    expect_identical(result$get(), testMatrix1)
    
    result$set(testMatrix2)
    expect_identical(result$get(), testMatrix2)
  })
  
  it("has working $setCache and $getCache", {
    result <- makeCacheMatrix(testMatrix1)
    result$setCache(correctSolution1)
    expect_identical(result$getCache(), correctSolution1)
    
    result$setCache(correctSolution2)
    expect_identical(result$getCache(), correctSolution2)
    
    result$set(testMatrix3)
    expect_identical(result$getCache(), NULL)
  })
})
