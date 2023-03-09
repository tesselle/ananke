## Calibrate a single date
cal <- c14_calibrate(130, 20)
plot(cal)

## Calibrate multiple dates
BP <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)
plot(BP, panel.first = graphics::grid())

## Convert BP scale to CE
CE <- BP_to_CE(BP)
plot(CE)

## HPDI
hpdi(BP, level = 0.954)
hpdi(CE, level = 0.954)
