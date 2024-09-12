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

## Coerce to list
as.list(hdr95)

## Plot
plot(hdr95)
plot(cal)
