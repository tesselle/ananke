# Combine mutliple 14C dates ===================================================
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
  expected <- data.frame(
    groups = c("W-1571", "ANU-5", "C-800", "L-698D", "FSU-3", "Tx-44", "ANU-7"),
    ages = c(14650, 11700, 10860, 11840, 11245, 10700, 14253.1677018634),
    errors = c(500, 260, 410, 100, 450, 210, 190.324991749565),
    chi2 = c(NA, NA, NA, NA, NA, NA, 6.15790200138026),
    p = c(NA, NA, NA, NA, NA, NA, 0.104175631871266)
  )
  expect_equal(cmb, expected)

  cmb_null <- c14_combine(
    ages = polach1972$ages,
    errors = polach1972$errors,
    groups = NULL
  )
  expected <- data.frame(
    groups = "X",
    ages = 12068.7658594458,
    errors = 74.5434561375289,
    chi2 = 226.307054693181,
    p = 0
  )
  expect_equal(cmb_null, expected)

  cmb_missing <- c14_combine(
    ages = polach1972$ages,
    errors = polach1972$errors,
    groups = NA
  )
  cmb_empty <- c14_combine(
    ages = polach1972$ages,
    errors = polach1972$errors,
    groups = ""
  )
  expect_equal(cmb_missing, cmb_empty)

# Calibrate a single 14C date ==================================================
  ## IntCal20
  ## (OxCal v4.4: 5905-5595 calBP)
  intcal20 <- c14_calibrate(5000, 45, curves = "intcal20")
  r99 <- as.list(interval_hdr(intcal20, level = 0.997), calendar = BP())
  expect_equal(r99[[1]][1, ], data.frame(start = 5903, end = 5597, p = 1))

  ## IntCal13
  ## (OxCal v4.4: 5905-5603 calBP)
  intcal13 <- c14_calibrate(5000, 45, curves = "intcal13", from = 45000, to = 0)
  r99 <- as.list(interval_hdr(intcal13, level = 0.997), calendar = BP())
  expect_equal(r99[[1]][1, ], data.frame(start = 5904, end = 5603, p = 1))

  ## IntCal09
  ## (OxCal v4.4: 5906-5603 calBP)
  intcal09 <- c14_calibrate(5000, 45, curves = "intcal09", from = 45000, to = 0)
  r99 <- as.list(interval_hdr(intcal09, level = 0.997), calendar = BP())
  expect_equal(r99[[1]][1, ], data.frame(start = 5904, end = 5603, p = 1))

# Out of calibration range =====================================================
  verb <- getOption("ananke.verbose")
  options(ananke.verbose = TRUE)
  expect_warning(c14_calibrate(52000, 200, curve = "intcal20"), "is out of calibration range")
  expect_warning(c14_calibrate(50, 200, curve = "intcal20"), "is out of calibration range")
  expect_warning(c14_calibrate(50100, 200, curve = "intcal20"), "may extent out of calibration range")
  expect_warning(c14_calibrate(150, 200, curve = "intcal20"), "may extent out of calibration range")
  options(ananke.verbose = verb)

  out <- suppressWarnings(
    c14_calibrate(c(52000, 50100, 2000), c(200, 200, 200))
  )
  expect_identical(out@status, c(1L, 2L, 0L))

  # plot_cal_warnings <- function() plot(out, interval = FALSE)

# Calibrate multiple 14C dates =================================================
  cal <- c14_calibrate(
    values = c(5000, 4500),
    errors = c(45, 35),
    names = c("X", "Y")
  )

  expect_equal_to_reference(cal, file = "_snaps/c14_calibrate.rds")

  # plot_cal_CE <- function() plot(cal, panel.first = graphics::grid())

  # plot_cal_BP <- function() plot(cal, calendar = BP())

  # plot_cal_b2k <- function() plot(cal, calendar = b2k())

  # FIXME: check text
  expect_stdout(describe(cal))
