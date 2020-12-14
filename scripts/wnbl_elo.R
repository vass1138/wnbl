## ---------------------------
##
## Script name: wnbl_elo.R
##
## Purpose of script:
##   Compute ELO values for WNBL 2020 teams at end of regular season.
##   Compute probabilities for finals' matchups.
##
## Author: Dr. Emanuel Vassiliadis
##
## Date Created: 2020-12-15
##
## LinkedIn: https://www.linkedin.com/in/evassiliadis/
##
## ---------------------------
##
## Notes:
##   Based on NBL1 code.
##
## ---------------------------

rm(list = ls(all = TRUE))

library(here)
getwd()

library(tidyverse)
library(readr)

#
# Generate ELO scores from season game data.
# Use one round as test data (parameter to elo_model() function).
# All other data used for training.
#

#install.packages("elo")
library(elo)

map_margin_to_outcome <- function(margin, marg.min = -49, marg.max = 49){
  norm <- (margin - marg.min)/(marg.max - marg.min) 
  norm %>% pmin(1) %>% pmax(0)
}

map_outcome_to_margin <- function(prob, marg.min = -49, marg.max = 49){
    margin <- prob * (marg.max -marg.min) + marg.min
    margin %>% pmin(marg.max) %>% pmax(marg.min)
}

elo_model <- function(train_data,margin_lo,margin_hi,hga,k_factor) {

  # NBA advantage about 2.5-3.5
  elo.data <- elo.run(
    map_margin_to_outcome(margin,margin_lo,margin_hi) ~ adjust(home_team, hga) + away_team,
    k = k_factor,
    data = train_data
  )
  
  # https://cran.r-project.org/web/packages/elo/vignettes/elo.html
  # update proportional log of win margin
  # elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor +
  #           k(20*log(abs(points.Home - points.Visitor) + 1)), data = tournament)
  # 
  # different k values for home and away teams
  # k1 <- 20*log(abs(tournament$points.Home - tournament$points.Visitor) + 1)
  # elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + k(k1, k1/2), data = tournament)
  
  #
  as.data.frame(elo.data)
  
  # final elo
  final.elos(elo.data)
  
  return(elo.data)
}

elo_predict <- function(elo_data,test_data,margin_lo,margin_hi) {

  # predict final round
  test_data <- test_data %>%
    mutate(Prob = predict(elo_data, newdata = test_data))
  
  # compare actual vs predicted
  test_data <- test_data %>%
    mutate(pred_margin=round(map_outcome_to_margin(Prob,margin_lo,margin_hi)),
           result=ifelse((Prob>=0.5 & margin>=0) | (Prob<0.5 & margin<0),TRUE,FALSE)) %>%
    select(home_team,away_team,margin,Prob,pred_margin,result)
  
  return(test_data)
}

#
# MAIN
#

# read in non-zero results (no null results byregular season end)
games <- read_csv("../data/results2020.csv") %>%
  drop_na() %>%
  mutate(margin=home_score-away_score)

# Convert Sydney Uni Flames to SydneyUni Flames temporarily
games[games=="Sydney Uni Flames"] <- "SydneyUni Flames"

# scoring distribution
games %>%
  select_if(is.numeric) %>%
  summary()

# 
games %>%
  ggplot(aes(margin)) +
  geom_histogram()

games %>%
  ggplot(aes(margin)) +
  geom_boxplot()


#
# ELO
#

# Training/test split
set.seed(101) # Set Seed so that same sample can be reproduced in future also

# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(games), size = floor(0.8*nrow(games)), replace = F)
train <- games[sample, ]
test  <- games[-sample, ]

margin_sd <- train %>%
  summarize(sd(margin)) %>%
  as.numeric()

margin_lo <- -2*margin_sd
margin_hi <- 2*margin_sd 

# https://www.betfair.com.au/hub/tennis-elo-modelling/
# K computed for 13 games each team
# K = 250 / (13 + 5)^0.4 = 80


hga <- 3.5
k_factor <- 80
elo.wnbl <- elo_model(train,margin_lo,margin_hi,hga,k_factor)
as.data.frame(elo.wnbl)

elo.wnbl.final <- final.elos(elo.wnbl)
elo.wnbl.df <- cbind(read.table(text = names(elo.wnbl.final)),elo.wnbl.final)

library(data.table)
setDT(elo.wnbl.df,keep.rownames = TRUE)
elo.wnbl.df <- subset(elo.wnbl.df, select=-c(V1,V2))
colnames(elo.wnbl.df) <- c('Team','ELO')

elo.wnbl.df[elo.wnbl.df=="SydneyUni Flames"] <- "Sydney Uni Flames"

# Prediction

pred.wnbl <- elo_predict(elo.wnbl,test,margin_lo,margin_hi)

pred.wnbl %>%
  filter(result==TRUE) %>%
  summarise(n()/nrow(pred.wnbl))

#
# Before we plot ELO, associate with final standings

standings <- read_csv("../data/standings_28356.csv")
head(standings)

teams <-read_csv("../data/teams_mapping_28356.csv")
head(teams)

# join teams and standings on TeamID, select Position
teams <- teams %>%
  left_join(standings,by="TeamID") %>%
  rename(Team = Team.x) %>%
  select(Team,Position)

# Rename 'SydneyUni' here, or revert name in elo.wnbl.df
elo.wnbl.df <- elo.wnbl.df %>%
  left_join(teams,by="Team")

# Let's compare final ELO scores with final standings

# plot1 <- ggplot(elo.wnbl.df,aes(x=Team,y=ELO)) +
plot1 <- ggplot(elo.wnbl.df,aes(x=reorder(Team,-Position),y=ELO)) +
  geom_col() +
  geom_label(aes(label = round(ELO)), fill="lightblue",hjust = "center") +
  geom_hline(yintercept=1500,linetype="solid",color="red",size=1) +
  xlab("Team") +
  ylab("ELO") +
  ylim(1300,1700) +
  ggtitle("WNBL 2020", subtitle="Final ELO score vs Final Standing") +
  theme(axis.text.x = element_text(angle = 0),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle=element_text(hjust = 0.5)) + 
  coord_flip()

plot1

ggsave("wnbl_elo_2020.png")

# finals predictions

finals <-  read_csv("data/finals2020.csv") %>%
  distinct(.,home_team,away_team)

finals$margin <- 0

pred.wnbl.finals <- elo_predict(elo.wnbl,finals,margin_lo,margin_hi)

# copy probabilities to draw.io bracket diagram
pred.wnbl.finals


