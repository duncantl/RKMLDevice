# http://www.engr.udayton.edu/weather/citylistUS.htm

if(FALSE) {
with(sf, plot(dates, temp, type = "l"))
abline(h = mean(sf$temp, na.rm = TRUE), col = "red", lty = 2, lwd = 2)

bwplot(  temp ~ ordered(months(dates), unique(months(dates))), SFTemperatures)

with(subset(sf, dates >= as.Date("1-1-2004", "%d-%m-%Y")),
             { plot(dates, temp, type = "l")
               lines(supsmu(dates, temp), col = "red", lwd = 3)
             })
}


