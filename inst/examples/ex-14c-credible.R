## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## Credible intervals
crd68 <- interval_credible(cal, level = 0.683)
crd95 <- interval_credible(cal, level = 0.954)
crd99 <- interval_credible(cal, level = 0.997)

## Coerce to data.frame
as.data.frame(crd95, calendar = BC())

## Plot
plot(cal, interval = "credible")
