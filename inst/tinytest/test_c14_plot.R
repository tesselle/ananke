Sys.setenv(LANGUAGE = "en") # Force locale

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  data("ksarakil")
  cal <- c14_calibrate(
    values = c(5000, 4500, 3000),
    errors = c(45, 35, 35),
    names = c("X", "Y", "Z")
  )

  ## Calibrate
  plot_cal_fixed_decr <- function() plot(cal, level = 0.68, fixed = TRUE, decreasing = TRUE)
  expect_snapshot_plot(plot_cal_fixed_decr, "plot_cal_fixed_decr")

  plot_cal_fixed_incr <- function() plot(cal, level = 0.68, fixed = TRUE, decreasing = FALSE)
  expect_snapshot_plot(plot_cal_fixed_incr, "plot_cal_fixed_incr")

  plot_cal_pos_decr <- function() plot(cal, level = 0.68, fixed = FALSE, decreasing = TRUE)
  expect_snapshot_plot(plot_cal_pos_decr, "plot_cal_pos_decr")

  plot_cal_pos_incr <- function() plot(cal, level = 0.68, fixed = FALSE, decreasing = FALSE)
  expect_snapshot_plot(plot_cal_pos_incr, "plot_cal_pos_incr")
}
