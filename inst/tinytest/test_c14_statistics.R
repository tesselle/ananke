Sys.setenv(LANGUAGE = "en") # Force locale

# Calibrate multiple 14C dates =================================================
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## Quantiles
expected <- structure(
  c(-4034, -4034, -3876, -3282, -3778, -3210, -3729, -3144, -3540, -2916),
  dim = c(2L, 5L),
  dimnames = list(c("X", "Y"), c("0%", "25%", "50%", "75%", "100%"))
)
expect_identical(quantile(cal), expected)

## Median
expect_identical(median(cal), c(X = -3778, Y = -3210))

## Mean
expect_identical(ananke:::round_values_stuiver(mean(cal)), c(X = -3795, Y = -3210))

# Sample 14C dates =============================================================
spl <- c14_sample(cal, n = 100)
expect_identical(dim(spl), c(100L, 2L))

# Round 14C dates ==============================================================
## Stuiver & Polach (1977), p. 362
expect_identical(ananke:::round_values_stuiver(c(8234, 42786)), c(8230, 42800))
expect_identical(ananke:::round_errors_stuiver(c(256, 2322)), c(260, 2300))
