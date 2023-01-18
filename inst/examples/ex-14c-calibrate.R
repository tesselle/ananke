## Calibrate a single date
calBP <- c14_calibrate(130, 20)
plot(calBP)

## Convert BP scale to CE
calCE <- BP_to_CE(calBP)
plot(calCE)

## Calibrate multiple dates
cal <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)
plot(cal, panel.first = graphics::grid())
