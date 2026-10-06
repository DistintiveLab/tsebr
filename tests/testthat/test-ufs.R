test_that("UFs validas e invalidas", {
  expect_identical(.tse_ufs("df"), "DF")
  expect_length(.tse_ufs("all"), 28L)   # 27 UFs + ZZ
  expect_error(.tse_ufs("XX"), "invalida")
  expect_identical(.tse_ufs(c("DF", "SP")), c("DF", "SP"))
})
