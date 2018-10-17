# Zach Terner
# September 17, 2018
# Code to demonstrate nflscrapR package
# Taken from https://github.com/maksimhorowitz/nflscrapR
# And https://github.com/maksimhorowitz/nflscrapR/blob/master/vignettes/nflscrapR-vignette.Rmd
# For pbp data, think about doing this: https://www.r-bloggers.com/nfl-series/

library(devtools)
devtools::install_github(repo = "maksimhorowitz/nflscrapR")

library(nflscrapR)
library(tidyverse)
library(ggjoy)
library(ggplot2)
library(gridExtra)

###############################################################
# Get games from 2014 section

# Get games from 2014
games2014 <- season_games(Season = 2014)

# Look at the first few
head(games2014)

# Get all of the Seahawks games from 2014
sea.games <- games2014 %>%
  filter(home == "SEA" | away == "SEA")

# Print the first few
head(sea.games)


###############################################################

# Super Bowl 47 comparison
superbowl47 <- game_play_by_play(GameID = 2013020300)

# Explore dataframe dimensions
dim(superbowl47) 

# Look at number of offensive plays for each team
superbowl47 %>% 
  group_by(posteam) %>% 
  summarize(offensiveplays = n()) %>%
  filter(., posteam != "")

# As seen above the Ravens ran more plays than the 49ers. It would be interesting to explore what this play differential means in terms of time of possession, scoring opportunities, and play selections. Some ideas on how to proceed are below:
  
# Examining time of possession to see if one team dictated pace of play
# Dive into expected point and win probability values to see the true value of each teams possession time
# Analyze run pass breaks downs of both teams to see play calling tendencies
# Add more statistics on the "play" level such as yards per play, points per play, or play duration

sb_team_summary_stats <- 
  superbowl47 %>% 
  group_by(posteam) %>% 
  summarize(offensiveplays = n(), 
            avg.yards.gained = mean(Yards.Gained, na.rm = TRUE),
            pointsperplay = max(PosTeamScore, na.rm = TRUE) / n(),
            playduration = mean(PlayTimeDiff)) %>%
  filter(., posteam != "") %>% 
  as.data.frame() 

# Yards per play plot
plot_yards <- ggplot(sb_team_summary_stats, 
                     aes(x = posteam, y = avg.yards.gained)) +
  geom_bar(aes(fill = posteam), stat = "identity") +
  geom_label(aes(x = posteam, y = avg.yards.gained + .3, 
                 label = round(avg.yards.gained,2)),
             size = 4, fontface = "bold") +
  labs(title = "Superbowl 47: Yards per Play by Team",
       x = "Teams", y = "Average Yards per Play") +
  scale_fill_manual(values = c("#241773", "#B3995D")) +
  theme(plot.title = element_text(hjust = .5, face = "bold"))

# Points per play plot
plot_points <- ggplot(sb_team_summary_stats, 
                      aes(x = posteam, y = pointsperplay)) +
  geom_bar(aes(fill = posteam), stat = "identity") +
  geom_label(aes(x = posteam, y = pointsperplay + .05, 
                 label = round(pointsperplay,5)),
             size = 4, fontface = "bold") +
  labs(title = "Superbowl 47: Points per Play by Team",
       x = "Teams", y = "Points per Play") +
  scale_fill_manual(values = c("#241773", "#B3995D")) +
  theme(plot.title = element_text(hjust = .5, face = "bold"))

# Play duration plot
plot_time <- ggplot(sb_team_summary_stats, 
                    aes(x = posteam, y = playduration)) +
  geom_bar(aes(fill = posteam), stat = "identity") +
  geom_label(aes(x = posteam, y = playduration + .05, 
                 label = round(playduration,2)),
             size = 4, fontface = "bold") +
  labs(title = "Superbowl 47: Average Play Time Duration \n by Team",
       x = "Teams", y = "Average Play Duration") +
  scale_fill_manual(values = c("#241773", "#B3995D"))+
  theme(plot.title = element_text(hjust = .5, face = "bold"))

# Plotting the three charts together 
grid.arrange(plot_yards, plot_points, plot_time, ncol =2)

###############################################################
# Play by play section

# Get play-by-play data but this seems to not work.
pbp_2009 <- season_play_by_play(2009)
pbp_2010 <- season_play_by_play(2010)
pbp_2011 <- season_play_by_play(2011)
pbp_2012 <- season_play_by_play(2012)
pbp_2013 <- season_play_by_play(2013)
pbp_2014 <- season_play_by_play(2014)
pbp_2015 <- season_play_by_play(2015)
pbp_2016 <- season_play_by_play(2016)

pbp_data <- bind_rows(pbp_2009, pbp_2010, pbp_2011, pbp_2012, 
                      pbp_2013, pbp_2014, pbp_2015, pbp_2016)

saveRDS(pbp_data, file = "pbp_nfl.Rds")

# Now filter down to only passing attempts, group by the season and passer,
# then calculate the number of passing attempts, total expected points added
# (EPA), EPA per attempt, then finally filter to only those with at least 50
# pass attempts:

# Get passing stats from play-by-play data
passing_stats <- pbp_data %>% 
  filter(PassAttempt == 1 & PlayType != "No Play" & !is.na(Passer)) %>% 
  group_by(Season, Passer) %>% 
  summarise(Attempts = n(), 
            Total_EPA = sum(EPA, na.rm = TRUE), 
            EPA_per_Att = Total_EPA/Attempts) %>% 
  filter(Attempts >= 50)

# Make a ggplot of it
ggplot(passing_stats, aes(x = EPA_per_Att, y = as.factor(Season))) + 
  geom_joy(scale = 3, rel_min_height = 0.01) + 
  theme_joy() + ylab("Season") + xlab("EPA per Pass Attempt") + 
  scale_y_discrete(expand = c(0.01, 0)) + 
  scale_x_continuous(expand = c(0.01, 0)) + 
  ggtitle("The Shifting Distribution of EPA per Pass Attempt") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 16))
