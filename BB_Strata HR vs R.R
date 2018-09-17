library(Lahman)
library(dplyr)
library(ggplot2)

#caculates the correlation between HR and Runs when BB is stratified

BB_Strata_Dataset <- Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(BB_Strata = round(BB/G, 1), HR_per_game = HR/G, R_per_game = R/G)%>%
  filter(BB_Strata >=2.8 & BB_Strata <= 3.6)
BB_Strata_Dataset%>%ggplot(aes(HR_per_game, R_per_game)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~BB_Strata)

BB_Strata_Dataset%>%group_by(BB_Strata)%>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))
