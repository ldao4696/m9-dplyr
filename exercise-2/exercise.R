# Exercise 2: Data Frame Practice with `dplyr`.
# Use a different appraoch to accomplish the same tasks as exercise-1

# Install devtools package: allows installations from GitHub
install.packages('devtools')
install.packages('dplyr')

# Install "fueleconomy" package from GitHub
devtools::install_github("hadley/fueleconomy")

# Require/library the fueleconomy package
library(fueleconomy)
library(dplyr)
# You should have have access to the `vehicles` data.frame

# Create a data.frame of vehicles from 1997
vehicles.1997 <- filter(vehicles, year == 1997)


# Use the `unique` function to verify that there is only 1 value in the `year` column of your new data.frame
unique(vehicles.1997$year)

# Create a data.frame of 2-Wheel Drive vehicles that get more than 20 miles/gallon in the city
wheel.2dr.20mi <- filter(vehicles, drive == "2-Wheel Drive", cty > 20)

# Of those vehicles, what is the vehicle ID of the vehicle with the worst hwy mpg?
worst.hwy.mpg.filter <- filter(wheel.2dr.20mi, min(hwy) == hwy)
worst.hwy.mpg <- select(worst.hwy.mpg.filter, id)

# Write a function that takes a `year` and a `make` as parameters, and returns 
# The vehicle that gets the most hwy miles/gallon of vehicles of that make in that year


# What was the most efficient honda model of 1995?


