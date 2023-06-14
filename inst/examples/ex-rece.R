\dontrun{
## Radiocarbon data from Bosch et al. 2015
data("ksarakil")

## Calibrate
cal <- c14_calibrate(
  ages = ksarakil$date,
  errors = ksarakil$error,
  names = ksarakil$code,
  curves = "marine13",
  reservoir_offsets = 53,
  reservoir_errors = 43,
  from = 50000, to = 0
)
plot(cal, flip = TRUE)

## RECE
tmp <- c14_ensemble(cal, n = 500)
plot(tmp, col = grDevices::hcl.colors(12, "Viridis", rev = TRUE))
}