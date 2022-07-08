test_that("Download latest version of autoLIM-Excel from GitHub", {
  #expect_equal(2 * 2, 4)
  expect_true(file.exists("autoLIM-Excel_4node_Winter.xlsx") == TRUE)
})

