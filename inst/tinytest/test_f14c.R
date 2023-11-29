# F14C to BP14C ================================================================
## Asymmetric 14C errors (van der Plicht and Hogg 2006)
c14_zero <- F14C_to_BP14C(-0.0052, 0.0006, asym = TRUE)
expect_equal(c14_zero$age, - 8033 * log(2 * 0.0006))
expect_equal(c14_zero$plus, Inf)
expect_equal(c14_zero$minus, Inf)

c14_2sigma <- F14C_to_BP14C(0.0002, 0.0006, asym = TRUE)
expect_equal(c14_2sigma$age, - 8033 * log(0.0002 + 2 * 0.0006))
expect_equal(c14_2sigma$plus, Inf)
expect_equal(c14_2sigma$minus, Inf)

## Symmetric 14C errors (Bronk Ramsey 2008)
c14_sym <- F14C_to_BP14C(0.0052, 0.0006, asym = FALSE)

# BP14C to F14C ================================================================
f14c <- BP14C_to_F14C(c14_sym$age, c14_sym$plus)
expect_equal(f14c, data.frame(value = 0.0052, error = 0.0006))
