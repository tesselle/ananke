## Calibrate a single date
cal <- c14_calibrate(300, 20)
plot(cal, panel.first = graphics::grid())

## Calibrate multiple dates
cal <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)
plot(cal, calendar = BP(), flip = TRUE)
plot(cal, interval = FALSE, ncol = 2)


\donttest{
## Out of 14C range?
plot(c14_calibrate(130, 20))
}
