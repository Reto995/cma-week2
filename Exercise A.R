# Aufgabe 1 

library("readr")
library("sf")

wildschwein_BE <- read_delim("dataset/wildschwein_BE_2056.csv", ",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056)

difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}


wildschwein_BE <- wildschwein_BE |> 
  group_by(TierID) |> 
  mutate(timelag = difftime_secs(lead(DatetimeUTC), DatetimeUTC))


n_distinct(wildschwein_BE$TierID)  # damit kann geschaut werden wie viele Personen getrackt wurden

Tracking_time <- wildschwein_BE |> 
  group_by(TierID) |> 
  summarise(
    Startzeit = min(DatetimeUTC,na.rm = TRUE),
    Endzeit = max(DatetimeUTC,na.rm = TRUE)
  ) |> 
  ungroup()

print(Tracking_time)

signifikante_lücken <- wildschwein_BE |> 
  filter(timelag > 1500)


# Task 3

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}



wildschwein_BE <- wildschwein_BE |> 
  group_by(TierID) |> 
  mutate(steplength = distance_by_element(lead(geometry), geometry))


# Task 4

wildschwein_BE <- wildschwein_BE |> 
  group_by(TierID) |> 
  mutate(speed = steplength / timelag)


# Task 5

wildschwein_sample <- wildschwein_BE |>
  filter(TierName == "Sabi") |> 
  head(100)

library(tmap)
tmap_mode("view")

tm_shape(wildschwein_sample) + 
  tm_dots()


wildschwein_sample_line <- wildschwein_sample |> 
  # dissolve to a MULTIPOINT:
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")


tmap_options(basemaps = "OpenStreetMap")

tm_shape(wildschwein_sample_line) +
  tm_lines() +
  tm_shape(wildschwein_sample) + 
  tm_dots()






















