Pb <- data.frame(
  x = c(18.23247, 18.22936, 18.23102), # Pb206/Pb204
  y = c(15.65199, 15.65216, 15.65097), # Pb207/Pb204
  z = c(38.5167, 38.51516, 38.51601)   # Pb208/Pb204
)

## Default reference values from Albarede & Juteau (1984)
pb_age(
  Pb,
  t0 = 3.8,
  x_star = 18.75, y_star = 15.63, z_star = 38.86,
  mu = 9.66, kappa = 3.90, th232 = 0.049475,
  u238 = 0.155125, u235 = 0.98485, u238_235 = 137.79
)

## Reference values from Albarede et al. (2012)
pb_age(
  Pb,
  t0 = 4.43,
  x_star = 18.75, y_star = 15.63, z_star = 38.83,
  mu = 9.66, kappa = 3.90, th232 = 0.049475,
  u238 = 0.155125, u235 = 0.98485, u238_235 = 137.79
)
