## Calibrate multiple dates
cal <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

head(as.data.frame(cal))
head(as.data.frame(cal, calendar = BP()))
head(as.data.frame(cal, calendar = NULL))
