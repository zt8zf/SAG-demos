# February 18, 2020
# Code for Rice's SMGT 430 class

# Load packages (may not need all of these)
library(tidyverse)  # for data wrangling
library(janitor)  # for data cleaning
library(rvest)      # for web scraping
library(corrplot)   # correlation plots

# Grab the data from basketball-reference
# Using template provided here: https://www.r-bloggers.com/analyzing-nba-player-data-i-getting-data/
# And code from Michael Lopez's slides: https://statsbylopez.files.wordpress.com/2016/01/lecture_8.pdf
# And information from Alex Franks' slides

# Get a URL to read in:
url <- "https://www.basketball-reference.com/leagues/NBA_2016_totals.html"
stats <- url %>% 
  read_html() %>% 
  html_table() %>% 
  .[[1]]

# Filter it to the players we want
nba.js <- stats %>%
  filter(Player %in% c("D'Angelo Russell", "Mario Hezonja", "Devin Booker", "Kelly Oubre", "Terry Rozier", "Jerian Grant")) %>%
  select(Player, `3P`, `3PA`, `3P%`, FG, FGA, `FG%`)

# Set the variables to be numeric
nba.js$`3P` <- as.numeric(nba.js$`3P`)
nba.js$`3PA` <- as.numeric(nba.js$`3PA`)
nba.js$`3P%` <- as.numeric(nba.js$`3P%`)
nba.js$`FG%` <- as.numeric(nba.js$`FG%`)
nba.js$FGA <- as.numeric(nba.js$FGA)
nba.js$FG <- as.numeric(nba.js$FG)

# Define k
k <- nrow(nba.js)

k

# Calculate p.bar
p.bar <- mean(nba.js$`3P%`)

p.bar

#Calculate p.hat
p.hat <- nba.js$`3P%`

p.hat

# Get denom (SS of p bar)
ss.p.bar <- sum((p.hat - p.bar)^2)

ss.p.bar

# Get variance
sigma.sq <- p.bar*(1-p.bar)/176 ##very rough approx

# Try another way of getting variance
# bears little difference
# var2 <- mean(nba.js$`3P%`)*(1 - mean(nba.js$`3P%`))/mean(nba.js$`3PA`)

sigma.sq

# Calculate c
c <- 1 - (k-3)*sigma.sq/ss.p.bar

c

#Calculate js estimate
js.est <- p.bar + c*(p.hat - p.bar)

# Compare those with p.ats
js.est

p.hat

# Get career totals
nba.js$Player

# Make sure to put them in the right order with the players
# These were taken manually from basketball-reference.com on 2/10
nba.js$career3 <- c(.356,.324,.318,.331,.364,.357)

# Round to three digits for display purposes
nba.js$js.est <- round(js.est, 3)

head(nba.js)

# Define RMSE function to calculate RMSE
RMSE <- function(x, y){sqrt(mean((x-y)^2))}

# Calculate MSE with first season's shots and career 3
RMSE(nba.js$`3P%`, nba.js$career3)

# How does that compare to the JS estimator and career 3? 
RMSE(nba.js$js.est, nba.js$career3)
# JS does ~11% better in terms of RMSE.

# Define range of james-stein estimates
nbar <- range(nba.js$`3P%`)

# Make a blank plot without x axis
plot(y = seq(from = nbar[1], to = .38, length.out = 3),
     x = seq(from = 1, to = 3, length.out = 3), type = "n",
     xlab = "", ylab = "3P%", xaxt = 'n')

# Add points for all three points
points(x = rep(1,6), y = nba.js$`3P%`, col = "blue", pch = 19)

points(x = rep(2,6), y = nba.js$js.est, col = "orange", pch = 19)

points(x = rep(3,6), y = nba.js$career3, col = "purple", pch = 19)

# Add lines from season% to JS estimates
newvec <- rep(NA, 12)
newvec[seq(1, 11, by = 2)] <- nba.js$`3P%`
newvec[seq(2, 12, by = 2)] <- nba.js$js.est
for (i in 1:6){
  lines(x = c(1,2), y = c(newvec[1+2*(i-1)], newvec[2*(i)]) )
}

# Add lines from JS estimates to career 3p%
newvec2 <- rep(NA, 12)
newvec2[seq(1, 11, by = 2)] <- nba.js$js.est
newvec2[seq(2, 12, by = 2)] <- nba.js$career3
for (i in 1:6){
  lines(x = c(2,3), y = c(newvec2[1+2*(i-1)], newvec2[2*(i)]),lty = 2 )
}

# Add ticks
axis(side = 1, at = c(1:3),tick = T, labels = c("Season 1 3p%", "JS Estimator", "Career 3p%"))

# Title the plot
title("3-point shooting percentage")