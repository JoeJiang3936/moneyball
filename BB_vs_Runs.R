library(Lahman)

# Scatter plot showing the relationship between SB and runs scored per game

Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(Runs_per_game = R/G, BB_per_gamme = BB/G)%>%
  ggplot(aes(BB_per_gamme, Runs_per_game, col=teamID)) +
  geom_point()

