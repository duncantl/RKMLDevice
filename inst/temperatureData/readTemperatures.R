readTemperatureData = 
function(city, state,
          from = sprintf("http://www.engr.udayton.edu/faculty/jkissock/gsod/%s%s.txt",
                             toupper(city), toupper(state)))
{  
  sf = read.table(url(from))
  names(sf) = c("yearCount", "dayOfMonth", "year", "temp")
  sf$temp[sf$temp < -90] = NA

  start = as.Date("1-1-1995", "%d-%m-%Y")
  sf$dates = seq(start, length = nrow(sf), by = "day")

  sf
}

if(FALSE) {
SeattleTemperatures = readTemperatureData(from = "file:///Users/duncan/WASEATTL.txt")
PortlandTemperatures = readTemperatureData(from = "file:///Users/duncan/ORPORTLA.txt")
}
