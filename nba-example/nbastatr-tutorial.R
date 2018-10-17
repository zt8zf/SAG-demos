# Write basketball scraping tutorial here
# https://github.com/abresler/nbastatR
# http://rstudio-pubs-static.s3.amazonaws.com/11288_111663babc4f44359a35b1f5f1a22b89.html
# https://github.com/Fossj117/NBAdata
# https://www.r-bloggers.com/fun-with-advanced-nba-stats%EF%BB%BF-how-to-collect-data/

# Great documentation here:
# http://asbcllc.com/nbastatR/reference/index.html

# Good documentation here:
# https://www.rdocumentation.org/packages/nbastatR/versions/0.1.03
# Install the package
devtools::install_github("abresler/nbastatR")

# Install gganimate package
devtools::install_github('thomasp85/gganimate')

library(gganimate)
# If there's an issue with loading this library, close and reopen R
# https://github.com/wch/movies/issues/3
install.packages("magick")
library(magick)
library("nbastatR")
library(dplyr)
players.df <- get_nba_players()

# Filter those who were active last year
players.df.2017 <- players.df %>%
  filter(yearSeasonLast == 2017)

players.df.2017


# Get the season metrics leaders; choose Totals rather than PerGame.
pts.df <- get_seasons_metrics_league_leaders(seasons = 2005:2018, metric = "pts",
                                   season_types = "Regular Season", 
                                   modes = c("Totals"), #in modes, write ("PerGame")
                                   return_message = TRUE, nest_data = FALSE)

# Take a look at what is included here.
head(pts.df)

# Group by number of attempts.
pts.df$bin.fg2 <- cut(pts.df$fg2a, breaks = seq(from = 0, by = 100, length.out = 17)) 

# Check the bins
head(pts.df$bin.fg2)

# And print some output to see it
pts.df %>% select(slugSeason, fg2a, fg2m, pctFG2) %>% print(n = 12)

# Get just 2004 season
pts.df.2004 <- pts.df %>% 
  filter(slugSeason == "2004-05") %>%
  select(pctFG2, bin.fg2)

# Now make a ggplot of the binned histogram
ggplot(pts.df.2004, aes(pctFG2, fill = bin.fg2)) + 
  #call geom_histogram with position="stack" to stack the bars
  geom_histogram(position = "stack")

# Can see regression to the mean.

# Convert slugSeason to a numeric season so that 
pts.df$season <- as.numeric(substr(pts.df$slugSeason, start = 1, stop = 4))

p <- ggplot(pts.df, aes(pctFG2, fill = bin.fg2)) + 
  #call geom_histogram with position="stack" to stack the bars
  geom_histogram(position = "stack") + 
  labs(title = 'Year: {floor(frame_time)}', x = '2pt FG%', y = 'count') +
  transition_time(season) + 
  ease_aes('linear') + 
  theme_bw()

p

# Save using anim_save; saves the last rendered animation
anim_save(filename = 'season-shot-fg2per-newtheme.gif')
