
library("readr")

caro_60 <-  read_delim("dataset/caro60.csv" , ",")

caro <- st_as_sf(caro_60, coords = c("E", "N"), crs = 2056)


library("readr")
library("sf")
library("dplyr")

difftime_secs <- function(x, y){
  as.numeric(difftime(x, y, units = "secs"))
}

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}


caro <- read_delim("dataset/caro60.csv", ",") |>
  st_as_sf(coords = c("E","N"), crs = 2056) |> 
  select(DatetimeUTC)

caro <- caro |> 
  mutate(
    timelag = difftime_secs(lead(DatetimeUTC, n = 2), DatetimeUTC),
    steplength = distance_by_element(lead(geometry, n = 2), geometry)
  ) |> 
  mutate(speed = steplength / timelag) 


head(caro)

# Task 2


caro120 <- caro |> 
  mutate(
    timelag2 = difftime_secs(lead(DatetimeUTC, n = 4), DatetimeUTC),
    steplength2 = distance_by_element(lead(geometry, n = 4), geometry)
  ) |> 
  mutate(speed2 = steplength2 / timelag2)

head(caro120)


caro120 |> 
  # drop geometry and select only specific columns
  # to display relevant data only
  st_drop_geometry() |> 
  select(timelag2, steplength2, speed2) |> 
  head()


caro <- caro120 |> 
  mutate(
    timelag3 = difftime_secs(lead(DatetimeUTC, n = 8), DatetimeUTC),
    steplength3 = distance_by_element(lead(geometry, n = 8), geometry)
  ) |> 
  mutate(speed3 = steplength3 / timelag3)

head(caro)


caro |> 
  # drop geometry and select only specific columns
  # to display relevant data only
  st_drop_geometry() |> 
  select(timelag3, steplength3, speed3) |> 
  head()

caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)


library(ggplot2)

ggplot(caro, aes(y = speed)) + 
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)


library(tidyr)

# before pivoting, let's simplify our data.frame
caro2 <- caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

caro_long <- caro2 |> 
  pivot_longer(c(speed, speed2, speed3))

head(caro_long)



ggplot(caro_long, aes(name, value)) +
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)






