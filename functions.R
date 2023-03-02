setDateTime <- function(dateTimeColumn, 
                        Table = FALSE) {
  datetime <- strptime(dateTimeColumn,
                       tz = parameters$timeZone,
                       format = "%HH %MM %OS")
  datetime <- format(datetime, format = "%H:%M:%OS", usetz = FALSE)
  return(datetime)
}

# datetime <- strptime(datetime_str, format = "%HH %MM %OS", tz = "UTC")
# datetime <- format(datetime, format = "%H:%M:%OS", usetz = FALSE)