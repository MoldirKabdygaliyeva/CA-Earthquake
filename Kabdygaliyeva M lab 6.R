#lab5
library(tidyverse)

data1 <- read.csv("Central-Asian-earthquake-dataset.csv")

dimensions <- dim(data1)
num_rows <- dimensions[1]
num_columns <- dimensions[2]
row_names <- rownames(data1)
column_names <- colnames(data1)

print(paste("Number of rows:", num_rows))
print(paste("Number of columns:", num_columns))
print("Row names:")
print(row_names)
print("Column names:")
print(column_names)

head(data1)

tail(data1)

total_na <- sum(is.na(data1))
print(paste("Total NA values in the dataset:", total_na))

na_by_column <- sapply(data1, function(x) sum(is.na(x)))
print("NA values by column:")
print(na_by_column)

summary(data1)

install.packages("tidyverse")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

#lab6
library(dplyr)
data2 <- distinct(data1)

na_summary <- sapply(data2, function(x) sum(is.na(x)))
print(na_summary)

total_rows <- nrow(data2)

data2 <- data2 %>% mutate(across(where(is.numeric), ~ifelse(sum(is.na(.)) / total_rows < 0.1, replace_na(., mean(., na.rm = TRUE)), .)))

data2 <- data2 %>% select(where(~ sum(is.na(.)) / total_rows <= 0.5))

plot(data2$Longitude, data2$Latitude, 
     xlab = "Longitude", ylab = "Latitude", 
     main = "Earthquake Locations: Latitude vs Longitude", 
     pch = 19, col = "blue")

hist(data2$Magnitude, breaks = 30, 
     col = "skyblue", border = "black", 
     main = "Histogram of Earthquake Magnitudes", 
     xlab = "Magnitude", ylab = "Frequency")

boxplot(data2$Depth, 
        main = "Boxplot of Earthquake Depth", 
        ylab = "Depth (km)", col = "lightgreen")

barplot(table(data2$Country), 
        main = "Earthquake Occurrences by Country", 
        xlab = "Country", ylab = "Count", 
        las = 2, col = "lightblue")

pie(table(data2$Country), 
    main = "Proportion of Earthquakes by Country", 
    col = rainbow(length(unique(data2$Country))))
