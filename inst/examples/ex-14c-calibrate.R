## Calibrate a single date
cal <- c14_calibrate(300, 20)
plot(cal, panel.first = graphics::grid())

## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)
plot(cal, calendar = BP(), panel.first = graphics::grid())
plot(cal, interval = FALSE)
plot(cal[, 1, ], col.interval = "red")

plot(cal, density = FALSE, level = 0.68, lwd = 5)
plot(cal, density = FALSE, level = 0.95, lwd = 5)

\donttest{
## Out of 14C range?
out <- c14_calibrate(130, 20)
plot(out)
}
