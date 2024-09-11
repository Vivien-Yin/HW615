### liwen Yin ###

### Homework 2                                         ###
### GGPlot Basics ###

#Put your code in this file. Make sure you assign the relevant values to the correct variable names, which are given below. 
#Uncomment the variables as you assign your final values/functions/results to them.

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)# This loads the packages necessary to run your plots. Do not delete or comment this out.

### Exercise 1

sp <- read.csv("SPX-1Month.csv")
spx_plot1<- ggplot(sp, aes(x = Date, y = Close.Last, group = 1)) +
  geom_line() + 
  geom_point() 
print(spx_plot1)

spx_plot2<-ggplot(sp, aes(x = Date, y = Close.Last, group = 1)) +
  geom_line() + 
  geom_point() +
  labs(
    title = "S&P500-Closing Price over the Last Month",
    x = "Date",
    y = "Price"
  )+
  theme(axis.text.x=element_text(angle=45,hjust=1))
print(spx_plot2)

### Exercise 2

book<- read.csv("BookGenres.csv")
bookplot<-ggplot(book,aes(x = Category, y = Price, color = Season))+
  geom_point(alpha = 0.40) +
  labs(
    title = "The Price of Fiction Books by Genre and Season",
    x = "Genre",
    y = "Price($)"
  )+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=30,hjust=1))
print(bookplot) 

### Exercise 3

squirrel<- read.csv("squirrel.csv") |>
  group_by(Activities, Interactions.with.Humans) |>
  summarise(Count = n()) 
squirrelPlot<-ggplot(squirrel,aes(x = Activities, y = Count, fill = Interactions.with.Humans))+
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Squirrel Behavior in NYC Parks",
    x = "Squirrel's Activity during Observation",
    y = "Count"
  )+
  theme_grey()
print(squirrelPlot) 

### Exercise 4

BigMac<- read.csv("big_mac.csv")
# delete some elements cannot turn into numeric
BigMac <- BigMac[!is.na(as.numeric(BigMac$GDP.Per.Capita)), ]
BigMac$GDP.Per.Capita <- as.numeric(BigMac$GDP.Per.Capita)#x is character instead of numeric
bigMac<-ggplot(BigMac,aes(x = GDP.Per.Capita, y = dollar_price))+
  geom_point() +
  geom_smooth(method = "loess", color = "blue") + 
  labs(
    title = "Countries' GDP Per Capita compared to their Big Mac Index",
    x = "GDP per Capita in Dollars",
    y = "Dollar Price of a Big Mac"
  )+
  theme_grey()
print(bigMac) 
# log cannot deal with 0 or nagative number,so check:
str(BigMac)
summary(BigMac$GDP.Per.Capita)
BigMac$log_GDP.Per.Capita <- log(BigMac$GDP.Per.Capita)
logBigMac<- ggplot(BigMac, aes(x = log_GDP.Per.Capita, y = dollar_price)) +
  geom_point() + 
  geom_smooth(method = "loess", color = "blue") + 
  labs(
    title = "Countries' GDP Per Capita compared to their Big Mac Index",
    x = "Log of Dollar GDP per Capita",
    y = "Dollar Price of a Big Mac"
  ) +
  theme_grey()
print(logBigMac) 

