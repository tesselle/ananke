## IntCal20
intcal20 <- c14_curve("intcal20")
head(intcal20[[1]])

## IntCal
intcal <- c14_curve(c("intcal09", "intcal13", "intcal20"))
lapply(X = intcal, FUN = head)
