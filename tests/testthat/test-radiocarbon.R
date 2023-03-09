test_that("Combine mutliple 14C dates", {
  ## Replicate Ward and Wilson (1978), p. 28
  polach1972 <- data.frame(
    samples = c("ANU-7", "ANU-7", "ANU-7", "W-1571", "ANU-5",
                "C-800", "L-698D", "FSU-3", "Tx-44"),
    ages = c(14550, 15000, 13700, 14650, 11700, 10860, 11840, 11245, 10700),
    errors = c(270, 600, 300, 500, 260, 410, 100, 450, 210)
  )

  cmb <- c14_combine(
    ages = polach1972$ages,
    errors = polach1972$errors,
    groups = polach1972$samples
  )
  expect_snapshot(cmb)

  cmb_null <- c14_combine(
    ages = polach1972[1:4, ]$ages,
    errors = polach1972[1:4, ]$errors,
    groups = NULL
  )
  expect_snapshot(cmb_null)

  cmb_missing <- c14_combine(
    ages = polach1972[1:4, ]$ages,
    errors = polach1972[1:4, ]$errors,
    groups = NA
  )
  cmb_empty <- c14_combine(
    ages = polach1972[1:4, ]$ages,
    errors = polach1972[1:4, ]$errors,
    groups = ""
  )
  expect_equal(cmb_missing, cmb_empty)
})
test_that("Calibrate a single 14C date", {
  ## IntCal20
  ## (OxCal v4.4: 5905-5595 calBP)
  intcal20 <- c14_calibrate(5000, 45, curves = "intcal20")
  r99 <- hpdi(intcal20, level = 0.997)
  expect_equal(r99[[1]][1, ], c(5903, 5597, 1), ignore_attr = TRUE)

  ## IntCal13
  ## (OxCal v4.4: 5905-5603 calBP)
  intcal13 <- c14_calibrate(5000, 45, curves = "intcal13", from = 45000, to = 0)
  r99 <- hpdi(intcal13, level = 0.997)
  expect_equal(r99[[1]][1, ], c(5904, 5604, 1), ignore_attr = TRUE)

  ## IntCal09
  ## (OxCal v4.4: 5906-5603 calBP)
  intcal09 <- c14_calibrate(5000, 45, curves = "intcal09", from = 45000, to = 0)
  r99 <- hpdi(intcal09, level = 0.997)
  expect_equal(r99[[1]][1, ], c(5904, 5603, 1), ignore_attr = TRUE)
})
test_that("Calibrate multiple 14C dates", {
  BP <- c14_calibrate(
    ages = c(5000, 4500),
    errors = c(45, 35),
    names = c("X", "Y")
  )

  plot_cal_BP <- function() {
    plot(BP, panel.first = graphics::grid())
  }
  vdiffr::expect_doppelganger("plot_cal_BP", plot_cal_BP)

  plot_cal_CE <- function() {
    CE <- BP_to_CE(BP)
    plot(CE, panel.first = graphics::grid())
  }
  vdiffr::expect_doppelganger("plot_cal_CE", plot_cal_CE)
})
