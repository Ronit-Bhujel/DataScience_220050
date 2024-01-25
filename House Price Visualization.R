library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library("scales")

setwd("C:/Users/hacki/OneDrive/Desktop/Ronit_Bhujel_220050")

#importing the cleaned house prices
cleaned_houseprices= read_csv('Cleaned Data/Cleaned House Prices.csv') 

#-----------2022 House Price Box plot-----------#

#grouping the cleaned house prices by county , towns and DOT and showing the average price for each group
Grouped_houseprice = cleaned_houseprices%>% 
  group_by(`Town/City`,District,County,`Date of Transfer`) %>% 
  summarise(`Average Price`= mean(Price)) %>% 
  ungroup(`Town/City`,District,County,`Date of Transfer`) 

#creating box plot to visualize average house prices in Kent and Surrey in 2022
Grouped_houseprice %>% 
  filter(`Date of Transfer`==2022) %>% #filtering to show only house price data of 2022
  group_by(County) %>% #grouping by county since we are comparing counties only
  ggplot(aes(x = County, y = `Average Price`, fill=County)) + #setting x-axis and y-axis values
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,300000))+ #setting limits and breaks
  geom_boxplot() + #specifying the type of plot we need
  labs(title="2022 Average House Prices By County Box Plot") + #setting label for the chart
  scale_fill_manual(values = c("red","blue"))

#-----------2022 Average House Price Bar Chart-----------#

#creating bar chart to visualize average house prices in Kent and Surrey
Grouped_houseprice %>% 
  filter(`Date of Transfer`==2022) %>% #filtering to show only house price data of 2022
  group_by(County) %>% #grouping by county since we are comparing counties only
  ggplot(aes(x = County, y = `Average Price`, fill= County)) + #defining x-axis and y-axis values
  geom_bar(stat = "identity") + #using average prices as height of the bar
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,300000))+ #setting limits and breaks
  labs(title = "2022 Average House Prices Barchart") +
  scale_fill_manual(values = c("red","blue"))

#-----------2019-2022 Average House Line Graph-----------#

#grouping the cleaned house prices by county and year and showing the average price for each group
Grouped_houseprice2 = cleaned_houseprices%>% 
  group_by(County,`Date of Transfer`) %>% 
  summarise(`Average Price`= mean(Price)) 

#creating line graph of average house prices from 2021-2022
Grouped_houseprice2 %>%
  filter(`Date of Transfer`==2019 | `Date of Transfer`==2020 | `Date of Transfer`==2021 |`Date of Transfer`==2022) %>% #filtering to show only house price data of 2019,2020,2021,2022
  group_by(County, `Date of Transfer`) %>%  #grouping by county and date of transfer since we are comparing prices of counties year after year
  ggplot( aes(x = `Date of Transfer`, y = `Average Price`, group = County, color = County)) + #defining x-axis and y-axis values and colors of line
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size = 2, color = "brown") + #defining point size and color
  scale_y_continuous(limits=c(0,700000), breaks = seq(0,700000,100000), labels = label_number()) + #defining limits, breaks and setting label as number instead of scientific notation
  labs(title = "2019-2022 Average House Prices Line Graph", #defining labels 
       x = "Year",
       y = "Average Price")





