test_that("Download latest version of autoLIM-Excel from GitHub", {
  #expect_equal(2 * 2, 4)
  expect(file.exists("autoLIM-Excel 4node Winter.xlsx") == TRUE)
})

