## Calibrate multiple dates
cal <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## HDR
interval_hdr(cal, level = 0.683)
interval_hdr(cal, level = 0.954)
interval_hdr(cal, level = 0.997)
