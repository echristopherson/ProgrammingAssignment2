describe("cacheSolve", {
  testMatrix1 <- matrix(c(1,0,5,2,1,6,5,4,0), nrow = 3, ncol = 3)
  correctSolution1 <- solve(testMatrix1)
  
  testMatrix2 <- matrix(c(5,4,6,3,2,1,7,9,8), nrow = 3, ncol = 3)
  correctSolution2 <- solve(testMatrix2)

  describe("called on object without cached solution", {
    it("should return correct result", {
      matrixObject <- makeCacheMatrix(testMatrix1)
      expect_identical(cacheSolve(matrixObject), correctSolution1)
    })
  })
  
  describe("called on object with cached solution", {
    it("should return correct result", {
      matrixObject <- makeCacheMatrix(testMatrix2)
      matrixObject$setCache(correctSolution2)
      expect_identical(cacheSolve(matrixObject), correctSolution2)
    })
  })
})