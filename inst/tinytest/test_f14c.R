Sys.setenv(LANGUAGE = "en") # Force locale

# F14C to BP14C ================================================================
## Asymmetric 14C errors (van der Plicht and Hogg 2006)
## F14C < 0
c14_zero <- F14C_to_BP14C(-0.0052, 0.0006, asym = TRUE)
expect_identical(c14_zero$age, -8033 * log(2 * 0.0006))
expect_identical(c14_zero$plus, NA_real_)
expect_identical(c14_zero$minus, NA_real_)

## F14C < 2 * sigma
c14_2sigma <- F14C_to_BP14C(0.0002, 0.0006, asym = TRUE)
expect_identical(c14_2sigma$age, -8033 * log(0.0002 + 2 * 0.0006))
expect_identical(c14_2sigma$plus, NA_real_)
expect_identical(c14_2sigma$minus, NA_real_)

## F14C > 2 * sigma
c14_asym <- F14C_to_BP14C(0.0052, 0.0006, asym = TRUE, rounding = "stuiver")
expect_equal(c14_asym, data.frame(age = 42200, plus = 980, minus = 880))

## Symmetric 14C errors (Bronk Ramsey 2008)
c14_sym <- F14C_to_BP14C(0.0052, 0.0006, asym = FALSE, rounding = "stuiver")
expect_equal(c14_sym, data.frame(age = 42200, plus = 930, minus = 930))

# BP14C to F14C ================================================================
c14_sym <- F14C_to_BP14C(0.0052, 0.0006, asym = FALSE)
f14c <- BP14C_to_F14C(c14_sym$age, c14_sym$plus)
expect_equal(f14c, data.frame(value = 0.0052, error = 0.0006))

# Round 14C dates ==============================================================
## Stuiver & Polach (1977), p. 362
expect_identical(ananke:::round_values_stuiver(c(8234, 42786)), c(8230, 42800))
expect_identical(ananke:::round_errors_stuiver(c(256, 2322)), c(260, 2300))
