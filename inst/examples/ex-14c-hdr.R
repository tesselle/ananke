## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## HDR
hdr68 <- interval_hdr(cal, level = 0.683)
hdr95 <- interval_hdr(cal, level = 0.954)
hdr99 <- interval_hdr(cal, level = 0.997)

## Coerce to data.frame
as.data.frame(hdr95, calendar = BC())

## Plot
plot(cal, interval = "hdr")
