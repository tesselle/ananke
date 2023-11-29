## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## Statistics
quantile(cal)
median(cal)
mean(cal)

## Plot
plot(cal, calendar = CE())

## Need to set 'calendar'
abline(v = median(cal, calendar = CE()), lty = 2, col = "blue")
abline(v = mean(cal, calendar = CE()), lty = 2, col = "red")
