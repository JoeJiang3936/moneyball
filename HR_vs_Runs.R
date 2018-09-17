library(Lahman)

# Scatter plot showing the relationship between HR and runs score per game

Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(Runs_per_game = R/G, HR_per_gamme = HR/G)%>%
  ggplot(aes(HR_per_gamme, Runs_per_game, col=teamID)) +
  geom_point()