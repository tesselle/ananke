## Calibrate a single date
cal <- c14_calibrate(300, 20)

## Statistics
median(cal)
mean(cal)

## Plot
plot(cal, panel.first = graphics::grid())

## Need to set 'calendar' to NULL
abline(v = median(cal, calendar = NULL), lty = 2, col = "blue")
abline(v = mean(cal, calendar = NULL), lty = 2, col = "red")
