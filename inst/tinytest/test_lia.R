# Geological model age =========================================================
Pb <- data.frame(
  x = c(18.23247, 18.22936, 18.23102), # Pb206/Pb204
  y = c(15.65199, 15.65216, 15.65097), # Pb207/Pb204
  z = c(38.5167, 38.51516, 38.51601)   # Pb208/Pb204
)

ratios <- pb_age(Pb)
expect_equal_to_reference(ratios, file = "_snaps/lia.rds")

colnames(Pb) <- c("a", "y", "z")
expect_error(pb_age(Pb), "does not have components")
