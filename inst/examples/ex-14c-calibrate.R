## Calibrate a single date
cal <- c14_calibrate(300, 20)
plot(cal)

## Calibrate multiple dates
cal <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)
plot(cal, panel.first = graphics::grid(), calendar = BP())

plot(cal, density = TRUE, interval = FALSE)
plot(cal, density = FALSE, interval = TRUE, lwd = 4, tcl = 0)

\donttest{
## Out of 14C range?
plot(c14_calibrate(130, 20))
}
