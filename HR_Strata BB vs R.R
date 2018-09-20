#calculates the correlation between BB and HR, singles and HR, bb and singles
library(Lahman)
library(dplyr)
library(ggplot2)

Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(singles_per_game=(H-X2B-X3B-HR)/G, HR_per_game = HR/G, BB_per_game = BB/G)%>%
  summarize(cor(singles_per_game, HR_per_game), cor(singles_per_game, BB_per_game), cor(BB_per_game, HR_per_game))

#Stratify HR then look at the correlation between 

HR_Strata_Dataset <- Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(HR_Strata = round(HR/G, 1), BB_per_game = BB/G, R_per_game = R/G)%>%
  filter(HR_Strata>=.4 & HR_Strata <= 1.3)
HR_Strata_Dataset%>%ggplot(aes(R_per_game, BB_per_game)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_Strata)

HR_Strata_Dataset%>%group_by(HR_Strata)%>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(BB_per_game)/sd(R_per_game))
