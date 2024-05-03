
#Demo


difftime()

now <- as.POSIXct("2024-04-26 10:20:00")

later <- as.POSIXct("2024-04-26 11:35:00")


time_difference <- difftime(later, now, units = "secs")
time_difference

str(time_difference)

difftime_six <- function(later, now) {

as.numeric(difftime(later, now, units = "secs"))
}






# Lead an  lag

numbers <-  1:10
numbers

library("dplyr")

lead (numbers)

lag(numbers, 4, default = -9999)



# mutate

wildschwein <- tibble(
  TierID = c(rep("Hans", 5), rep("Klara", 5)),
  DatetimeUTC = rep(as.POSIXct("2015-01-01 00:00:00", tz = "UTC") + 0:4 * 15 * 60, 2)
)

wildschwein

now <-  wildschwein$DatetimeUTC

later <-  lead(now)
wildschwein$timelag <- difftime_six(later, now)

wildschwein


wildschwein <- group_by(wildschwein,TierID)


mutate(wildschwein, timelag2 = difftime_six(lead(DatetimeUTC), DatetimeUTC)
       )



wildschwein <-wildschwein |> 
  group_by(TierID) |> 
  mutate(
    tilmelag2 = difftime_six(lead(DatetimeUTC), DatetimeUTC)
    )

     # ctrl shift m   


wildschwein |> 
  group_by(TierID) |> 
  summarise(
    first_sample= min(DatetimeUTC)
  )













