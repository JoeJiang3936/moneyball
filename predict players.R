#linear regression for runs per game based on HR,BB,singles, doubles and triples per game
library(Lahman)
library(dplyr)
library(ggplot2)
library(tidyverse)

#using the lm function to get the coefs for HR, BB, singles, doubles and triples, the intercept too
#all information is assigned to fit and use the predict function to get the R_hat

fit <- Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(R= R/G, 
         singles = (H-X2B-X3B-HR)/G, 
         HR = HR/G, 
         BB = BB/G, 
         doubles = X2B/G, 
         triples = X3B/G)%>%
  lm(R~HR+BB+singles+doubles+triples, data=.)

#PA_per_game calcualated from data in the past three years

PA_per_game <- Batting%>%filter(yearID %in% 1999:2001)%>%
  group_by(teamID, yearID)%>%
  summarize(PA_per_game = sum(AB + BB)/162)%>%.$PA_per_game%>%mean


#Assume a player plays the whole game in 2002, what are the predicted runs scored?

Players <- Batting%>%filter(yearID %in% 1999:2001)%>%
  group_by(playerID)%>%
  mutate(PA = AB + BB)%>%
  summarize(G = sum(PA)/PA_per_game,
            HR = sum(HR)/G,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G, 
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G,
            PA = sum(PA))%>%
  filter(PA>=300)%>%
  ungroup()%>%
  mutate(R_hat = predict(fit, newdata = .))

#plot the predicted runs for all players

Players%>%ggplot(aes(R_hat,col ="red")) + geom_histogram(binwidth = 0.5)

#filter playes available in 2002 with their salaries added

Players <- Salaries%>%filter(yearID %in% 2002)%>%
  select(playerID, salary)%>%
  right_join(Players, by ="playerID") 

#add defensive positions

Players <- Fielding%>%filter(yearID %in% 2002)%>%
  filter(!POS %in% c("OF", "P"))%>%
  group_by(playerID)%>%
  top_n(1, G)%>%
  filter(row_number(G) == 1)%>%
  ungroup()%>%
  select(playerID, POS)%>%
  right_join(Players, by= "playerID")%>%
  filter(!is.na(POS), !is.na(salary))

#add names of the players

Players <- Master%>%select(playerID, nameFirst, nameLast, debut)%>%
  filter(debut < 1998)%>%
  right_join(Players, by = "playerID")

Players%>%select(nameFirst, nameLast, POS, salary, R_hat)%>%
  arrange(desc(R_hat))%>%top_n(10)

#plot salary and predicted runsfor each player

Players%>%ggplot(aes(salary, R_hat)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10()
  
  