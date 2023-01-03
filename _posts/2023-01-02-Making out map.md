---
layout: post
title: "Interactive maps in R"
subtitle: "Creating interactive maps with the Polity package"
background: '/img/posts/web-scraping/interactive-maps/interactive.png'
---

```{r setup, include=FALSE}

if(!is.null(dev.list())) dev.off()
cat("\014")
rm(list=ls(all=TRUE))

# Current directory 
setwd("C:/Users/Admin/Desktop/postdoc2/Forecasting default/Forecasting Course/Forecasting")

```

```{r}
library(forecast)
library(h2o)
library(TSstudio)
library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)
library(xts)
library(zoo)
library(UKgrid)
library(Quandl)
```


```{r}
df2 <- Quandl(code = "FRED/TOTALNSA",
    type = "raw",
    collapse = "monthly",
    order = "asc",
    end_date="2017-12-31")

data("iris", package = "datasets")
summary(iris)

```

```{r}
# Chapter 2 Code

# -------- Code Chank 1 --------
# The Sys.Date function get the current date from the system 
date <- Sys.Date() 
date
class(date)

# The Sys.time function get the current date and time from the system 
# The defualt format is POSIXct
time_ct <- Sys.time() 
time_ct
class(time_ct)

# Converting the POSIXct object to POSIXlt 
time_lt <- as.POSIXlt(time_ct)
time_lt
class(time_lt)

# -------- Code Chank 2 --------
# Unclass the POSIXct object
unclass(time_ct)
# Unclass the POSIXlt object
unclass(time_lt)

# -------- Code Chank 3 --------
# Quering for the second value 
unclass(time_lt)$sec
# Quering for the day of the year value
unclass(time_lt)$yday

# -------- Code Chank 4 --------
date <- as.Date("2014-5-12")
date
class(date)

# Creating POSIXct and POSIXlt objects
# Setting the time zone to EST
time_ct <- as.POSIXct("2014-5-12 20:05:35", tz = "EST")
time_ct
class(time_ct)

time_lt <- as.POSIXlt("2014-5-12 20:05:35", tz = "EST")
time_lt
class(time_lt)
```


```{r}

url <- "https://github.com/PacktPublishing/Hands-On-Time-Series-Analysis-with-R/blob/master/Chapter02/dates_formats.csv"



dates_df <- read.csv(url, stringsAsFactors = FALSE)

# -------- Code Chank 6 --------
str(dates_df)

```


