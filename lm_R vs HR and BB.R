library(Lahman)
library(dplyr)
library(ggplot2)

#calculates the coefficiency for predicting runs per game based on HR per game and 
#BB per game


Galton_heights <- Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(BB_per_game = BB/G, HR_per_game = HR/G, R_per_game = R/G)

fit_R_HR_BB <- lm(R_per_game~HR_per_game+BB_per_game, Galton_heights)
fit_R_HR_BB
class(fit_R_HR_BB)