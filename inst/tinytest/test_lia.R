Sys.setenv(LANGUAGE = "en") # Force locale

# Geological model age =========================================================
Pb <- data.frame(
  x = c(18.23247, 18.22936, 18.23102), # Pb206/Pb204
  y = c(15.65199, 15.65216, 15.65097), # Pb207/Pb204
  z = c(38.5167, 38.51516, 38.51601)   # Pb208/Pb204
)

# Reference values from Albarede & Juteau (1984)
ratios_1984 <- pb_age(Pb, t0 = 3.8,
                      x_star = 18.75, y_star = 15.63, z_star = 38.86,
                      mu = 9.66, kappa = 3.90, th232 = 0.049475,
                      u238 = 0.155125, u235 = 0.98485, u238_235 = 137.79)
expect_equal_to_reference(ratios_1984, file = "_snaps/pb_model_1984.rds")

# Reference values from Albarede et al. (2012)
ratios_2012 <- pb_age(Pb, t0 = 4.43,
                      x_star = 18.75, y_star = 15.63, z_star = 38.83,
                      mu = 9.66, kappa = 3.90, th232 = 0.049475,
                      u238 = 0.155125, u235 = 0.98485, u238_235 = 137.79)
expect_equal_to_reference(ratios_2012, file = "_snaps/pb_model_2012.rds")

colnames(Pb) <- c("a", "y", "z")
expect_error(pb_age(unclass(Pb)))
