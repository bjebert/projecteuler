dates <- seq(as.Date("1901-01-01"), as.Date("2000-12-31"), 1)
sum(wday(dates[mday(dates) == 1]) == 1)
