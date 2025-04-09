Sys.setenv(LANGUAGE = "en") # Force locale

# Combine mutliple 14C dates ===================================================
## Replicate Ward and Wilson (1978), p. 28
polach1972 <- data.frame(
  samples = c("ANU-7", "ANU-7", "ANU-7", "W-1571", "ANU-5",
              "C-800", "L-698D", "FSU-3", "Tx-44"),
  values = c(14550, 15000, 13700, 14650, 11700, 10860, 11840, 11245, 10700),
  errors = c(270, 600, 300, 500, 260, 410, 100, 450, 210)
)

cmb <- c14_combine(
  values = polach1972$values,
  errors = polach1972$errors,
  groups = polach1972$samples
)
expected <- data.frame(
  groups = c("W-1571", "ANU-5", "C-800", "L-698D", "FSU-3", "Tx-44", "ANU-7"),
  values = c(14650, 11700, 10860, 11840, 11245, 10700, 14253.1677018634),
  errors = c(500, 260, 410, 100, 450, 210, 190.324991749565),
  chi2 = c(NA, NA, NA, NA, NA, NA, 6.15790200138026),
  p = c(NA, NA, NA, NA, NA, NA, 0.104175631871266)
)
expect_equal(cmb, expected)

cmb_null <- c14_combine(
  values = polach1972$values,
  errors = polach1972$errors,
  groups = NULL
)
expected <- data.frame(
  groups = "X",
  values = 12068.7658594458,
  errors = 74.5434561375289,
  chi2 = 226.307054693181,
  p = 0
)
expect_equal(cmb_null, expected)

cmb_missing <- c14_combine(
  values = polach1972$values,
  errors = polach1972$errors,
  groups = NA
)
cmb_empty <- c14_combine(
  values = polach1972$values,
  errors = polach1972$errors,
  groups = ""
)
expect_equal(cmb_missing, cmb_empty)
