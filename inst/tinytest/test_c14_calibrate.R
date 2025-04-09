Sys.setenv(LANGUAGE = "en") # Force locale

# Calibrate a single 14C date ==================================================
## IntCal20
## (OxCal v4.4: 5905-5595 calBP)
intcal20 <- c14_calibrate(5000, 45, curves = "intcal20")
r99 <- as.list(interval_hdr(intcal20, level = 0.997), calendar = BP())
expect_equal(r99[[1]][1, ], data.frame(start = 5903, end = 5596, p = 1))

## IntCal13
## (OxCal v4.4: 5905-5603 calBP)
intcal13 <- c14_calibrate(5000, 45, curves = "intcal13", from = 45000, to = 0)
r99 <- as.list(interval_hdr(intcal13, level = 0.997), calendar = BP())
expect_equal(r99[[1]][1, ], data.frame(start = 5905, end = 5603, p = 1))

## IntCal09
## (OxCal v4.4: 5906-5603 calBP)
intcal09 <- c14_calibrate(5000, 45, curves = "intcal09", from = 45000, to = 0)
r99 <- as.list(interval_hdr(intcal09, level = 0.997), calendar = BP())
expect_equal(r99[[1]][1, ], data.frame(start = 5905, end = 5603, p = 1))

# Uncalibrate ==================================================================
expect_identical(c14_uncalibrate(5728, curve = "intcal20"), 4998.2)

# Calibrate multiple 14C dates =================================================
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)
expect_equal_to_reference(cal, file = "_snaps/c14_calibrate.rds")

# FIXME: check text
expect_stdout(describe(cal))

## SPD
spd <- c14_spd(cal)
expect_equal_to_reference(spd, file = "_snaps/c14_spd.rds")

# Validate =====================================================================
cal_out_lower <- c14_calibrate(52000, 200, curve = "intcal20")
expect_warning(ananke:::c14_validate(cal_out_lower, verbose = TRUE),
               "is out of calibration range")

cal_out_upper <- c14_calibrate(50, 200, curve = "intcal20")
expect_warning(ananke:::c14_validate(cal_out_upper, verbose = TRUE),
               "is out of calibration range")

cal_may_lower <- c14_calibrate(50100, 200, curve = "intcal20")
expect_warning(ananke:::c14_validate(cal_may_lower, verbose = TRUE),
               "may extent out of calibration range")

cal_may_upper <- c14_calibrate(150, 200, curve = "intcal20")
expect_warning(ananke:::c14_validate(cal_may_upper, verbose = TRUE),
               "may extent out of calibration range")

out <- c14_calibrate(c(52000, 50100, 2000), c(200, 200, 200))
expect_identical(out@status, c(1L, 2L, 0L))
