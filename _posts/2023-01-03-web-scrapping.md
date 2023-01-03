---
layout: post
title: "Web Scraping in R"
subtitle: "Scraping movie data from IMDB with rvest package"
background: '/img/posts/web-scraping/bg-cinema.png'
---

# Description
Normally, data transfer between programs is accomplished using data structures suited for automated processing by computers, not people. Such interchange formats and protocols are typically rigidly structured, well-documented, easily parsed, and minimize ambiguity. Very often, these transmissions are not human-readable at all.

## Screen scraping

Although the use of physical "dumb terminal" IBM 3270s is slowly diminishing, as more and more mainframe applications acquire Web interfaces, some Web applications merely continue to use the technique of screen scraping to capture old screens and transfer the data to modern front-ends.

```
install.packages("rvest")
install.packages("dplyr")
libray(rvest)
library(dplyr)

```

## The webpage
![IMDB page](/img/posts/web-scraping/imdb page.png)

## Web scraping IMDB with rvest
In this assignment, we will scrape IMDB with rvest package of R and create a dataframe with details of top 250 movies of IMDB. Top 250 movies of IMDB is a very popular category for movies and it would be useful to scrape details of these movies and make them available for analysis. Once dataset of 250 movies is available, we will establish one or two hypotheses through our analysis.

Below code mentions packages needed for executing code in this document, please ensure that these packages are installed if you are running these code chunks in R environment.