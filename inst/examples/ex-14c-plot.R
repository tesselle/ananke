## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## Specify calendar
plot(cal, calendar = BP())

## HDR intervals (default)
plot(cal, interval = "hdr", level = 0.95)

## Credible intervals
plot(cal, interval = "credible", level = 0.95)

## No intervals
plot(cal, interval = NULL)

## Intervals only
plot(cal, density = FALSE, level = 0.68, lwd = 5)
plot(cal, density = FALSE, level = 0.95, lwd = 5)

## Change colors
plot(cal[, 1, ], col.interval = "red")
