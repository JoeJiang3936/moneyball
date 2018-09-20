#linear regression for runs per game based on HR,BB,singles, doubles and triples per game
library(Lahman)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(brooms)

fit <- Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(R= R/G, singles = (H-X2B-X3B-HR)/G, HR = HR/G, BB = BB/G, doubles = X2B/G, triples = X3B/G)%>%
  lm(R~HR+BB+singles+doubles+triples, data=.)

coefs <- tidy(fit, conf.int = TRUE)


#predict runs per game for 2002

Teams%>%filter(yearID == 2002)%>%
  mutate(R = R/G, 
         singles = (H-X2B-X3B-HR)/G, 
         HR = HR/G, BB = BB/G, 
         doubles = X2B/G, 
         triples = X3B/G)%>%
  mutate(R_hat = predict(fit, newdata = .))%>%
  ggplot(aes(R_hat, R, label = teamID)) +
    geom_text(hjust = 0, nudge_x = .03, size = 3) +
    geom_point() +
    geom_abline()
