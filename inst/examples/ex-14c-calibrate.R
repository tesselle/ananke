## Calibrate a single date
cal <- c14_calibrate(300, 20)
plot(cal, panel.first = graphics::grid())

## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)
plot(cal, panel.first = graphics::grid())

\donttest{
## Out of 14C range?
out <- c14_calibrate(130, 20)
plot(out)
}
