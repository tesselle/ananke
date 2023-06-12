## Calibrate a single date
cal <- c14_calibrate(300, 20)

## Statistics
median(cal)
mean(cal)

## Plot
plot(cal, calendar = CE(), panel.first = graphics::grid())

## Need to set 'calendar'
abline(v = median(cal, calendar = CE()), lty = 2, col = "blue")
abline(v = mean(cal, calendar = CE()), lty = 2, col = "red")
