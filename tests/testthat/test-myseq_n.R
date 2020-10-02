test_that("mysq_n output", {
  x<- c(2, 4, 3)
  n<-4
  expect_identical(myseq_n(x = c(2, 4, 3), n = 4),2.5)
})
