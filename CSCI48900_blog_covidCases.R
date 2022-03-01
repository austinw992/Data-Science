
#setting up working directory for workspace to access files
setwd("C:\\Users\\Panda\\Desktop\\CSCI48900 Data Science Data Sheets")
getwd()


library(tidyverse)
library(ggplot2)

covid_data <- read.csv(file='covid_data.csv', header=T, stringsAsFactors=FALSE)
View(covid_data)
head(covid_data)

#isolate the data set into specific sections

#South Africa
Africa <- filter(covid_data, location=="South Africa")
Africa$total_cases[is.na(Africa$total_cases)] <-0
lmCases = lm(total_cases~new_cases, data = Africa)
View(lmCases)
head(lmCases)
Africa$date <- as.Date( Africa$date, '%Y-%m-%d')
#Africa$date[is.na(Africa$date)] <-0
#ggplot(data = Africa, aes(date,total_cases )) + geom_line()

plot(Africa$date, Africa$new_cases,
     col = "red", xlab = "date",
     ylab = "New Cases", main="AFRICA Cases")
#lines(Africa$date, Africa$stringency_index, col=28)
lines(Africa$date, Africa$new_deaths, col=29)
#legend(1, 95, legend=c("New Cases", "new Deaths"),
#       col=c("red", 29), lty=1:2, cex=0.8,
#       title="Line types", text.font=4, bg='lightblue')

#line(Africa$new_cases)
plot(Africa$date, Africa$stringency_index,
     col = "red", xlab = "date",
     ylab = "stringency Index", main="AFRICA Stringency Index")

summary(Africa$new_cases)


#United States of America
USA <- filter(covid_data, location=="United States")
USA$total_cases[is.na(USA$total_cases)] <-0
lmCases2 = lm(total_cases~new_cases, data = USA)
View(lmCases2)
head(lmCases2)

USA$date <- as.Date( USA$date, '%Y-%m-%d')
plot(USA$date, USA$new_cases,
     col = "red", xlab = "date",
     ylab = "New Cases", main="USA Cases")
lines(USA$date, USA$new_deaths, col=47)

plot(USA$date, USA$stringency_index,
     col = "red", xlab = "date",
     ylab = "stringency Index", main="USA Stringency Index")



#United Kingdom
UK <- filter(covid_data, location=="United Kingdom")
UK$total_cases[is.na(UK$total_cases)] <-0
lmCases3 = lm(total_cases~new_cases, data = UK)
View(lmCases3)
head(lmCases3)

UK$date <- as.Date( UK$date, '%Y-%m-%d')
plot(UK$date, UK$new_cases,
     col = "red", xlab = "date",
     ylab = "New Cases", main="United Kingdom Cases")
lines(UK$date, UK$new_deaths, col=97)

plot(UK$date, UK$stringency_index,
     col = "red", xlab = "date",
     ylab = "stringency Index", main="UNITED KINGDOM Stringency Index")

#Japan
JP <- filter(covid_data, location=="Japan")
JP$total_cases[is.na(JP$total_cases)] <-0
lmCases4 = lm(total_cases~new_cases, data = JP)
View(lmCases4)
head(lmCases4)

JP$date <- as.Date( JP$date, '%Y-%m-%d')
plot(JP$date, JP$new_cases,
     col = "red", xlab = "date",
     ylab = "New Cases", main="JAPAN Cases")
lines(JP$date, JP$new_deaths, col=70)

plot(JP$date, JP$stringency_index,
     col = "red", xlab = "date",
     ylab = "stringency Index", main="JAPAN Stringency Index")



summary(Africa$new_cases)
summary(USA$new_cases)
summary(UK$new_cases)
summary(JP$new_cases)

summary(Africa$total_cases)
summary(USA$total_cases)
summary(UK$total_cases)
summary(JP$total_cases)

#India
IN <- filter(covid_data, location=="India")
IN$total_cases[is.na(IN$total_cases)] <-0
lmCases5 = lm(total_cases~new_cases, data = IN)
View(lmCases5)
head(lmCases5)

IN$date <- as.Date( IN$date, '%Y-%m-%d')
plot(IN$date, IN$new_cases,
     col = "red", xlab = "Date Jan-30-2020 - Apr-26-2021",
     ylab = "New Cases", main="INDIA New Cases")
lines(IN$date, IN$new_deaths, col=70)

plot(IN$date, IN$stringency_index,
     col = "red", xlab = "date",
     ylab = "stringency Index", main="INDIA Stringency Index")

ggplot(IN, aes(x = date, y = new_cases, colour = 70)) +
        geom_point() +
        geom_line() + 
        scale_x_date(breaks = IN$date) + 
        theme(axis.text.x = element_text(angle = 90))
View(IN)

IN_2021 = IN[401:453,]

ggplot(IN_2021, aes(x = date, y = stringency_index, colour = stringency_index)) +
        geom_point() +
        geom_line() + 
        scale_x_date(breaks = IN_2021$date) + 
        theme(axis.text.x = element_text(angle = 90))
