NYC_BNB <- read.csv("AB_NYC_2019.csv", header = TRUE)

library(tidyr)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(readxl)
library(highcharter)
library(lubridate)
library(scales)
library(RColorBrewer)
library(wesanderson)
library(plotly)
library(shiny)
library(readxl)

NYC_BNB %>% 
  filter(!(is.na(neighbourhood_group))) %>% 
  filter(!(neighbourhood_group == "Unknown")) %>% 
  group_by(neighbourhood_group) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(neighbourhood_group, mean_price), y = mean_price, fill = neighbourhood_group)) +
  geom_col(stat ="identity", color = "black", fill="#649E16") +
  coord_flip() +
  theme_gray() +
  labs(x = "Neighbourhood Group", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("Mean Price comparision for each Neighbourhood Group", subtitle = "Price vs Neighbourhood Group") + 
  xlab("Neighbourhood Group") + 
  ylab("Mean Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())


NYC_BNB %>% 
  filter(!(is.na(room_type))) %>% 
  filter(!(room_type == "Unknown")) %>% 
  group_by(room_type) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(room_type, mean_price), y = mean_price, fill = room_type)) +
  geom_col(stat ="identity", color = "black", fill="#649E16") +
  coord_flip() +
  theme_gray() +
  labs(x = "Room Type", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("Mean Price comparision with all Room Types", subtitle = "Price vs Room Type") + 
  xlab("Room Type") + 
  ylab("Mean Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())



NYC_BNB %>% 
  filter(!(is.na(neighbourhood_group))) %>% 
  filter(!(neighbourhood_group == "Unknown")) %>% 
  group_by(neighbourhood_group) %>% 
  summarise(mean_reviews = mean(number_of_reviews, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(neighbourhood_group, mean_reviews), y = mean_reviews, fill = neighbourhood_group)) +
  geom_col(stat ="identity", color = "black", fill="#649E16") +
  coord_flip() +
  theme_gray() +
  labs(x = "Neighbourhood Group", y = "Number of reviews") +
  geom_text(aes(label = round(mean_reviews,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("Mean No of reviews comparision for each Neighbourhood Group", subtitle = "Number of Reviews vs Neighbourhood Group") + 
  xlab("Neighbourhood Group") + 
  ylab("Mean Number of Reviews") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())


library(plotly)

##d <- NYC_BNB[sample(nrow(NYC_BNB), "500"), ]

p <- plot_ly(
  NYC_BNB, x = ~number_of_reviews, y = ~price,
  # Hover text:
  text = ~paste("Price: ", price, '$<br>No of reviews:', number_of_reviews),
  color = ~number_of_reviews, size = ~price
)

p