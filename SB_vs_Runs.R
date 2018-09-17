library(Lahman)

# Scatter plot showing the relationship between SB and runs scored per game

Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(Runs_per_game = R/G, SB_per_gamme = SB/G)%>%
  ggplot(aes(SB_per_gamme, Runs_per_game, col=teamID)) +
  geom_point()

