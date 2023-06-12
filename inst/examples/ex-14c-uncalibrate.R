\dontrun{
## Calibrate multiple dates
cal <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## Uncalibrate
c14_uncalibrate(cal)
}
