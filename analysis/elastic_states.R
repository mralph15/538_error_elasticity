#Required packages
library(dplyr)
library(ggplot2)

#Load dataset
state_data <- read.csv("state_elastic.csv")

#Who won?
state_data <- state_data %>%
  mutate(WINNER=ifelse(Wiki_Margin < 0, "BIDEN", "TRUMP"))

#Graph1
state_data %>%
  ggplot(mapping=aes(x=Elasticity_Score, y=FTE_Wiki_DIFF)) +
  geom_point(aes(color=WINNER)) +
  scale_color_manual(values=c("blue", "red")) +
  geom_text(aes(label=State_Abv),vjust = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linetype="dotted", color="gray40") +
  geom_vline(xintercept=1,linetype="dashed", color = "gray40") +
  labs(title = "538 Model - Election Results Margins vs 2020 State Elasticity Scores",
       x= "2020 State Elasticity Score", y= "Margin Differences (538 Final Model - Wikipedia Results)")

#Graph2
state_data %>%
  filter(WINNER=="BIDEN") %>%
  ggplot(mapping=aes(x=Elasticity_Score, y=FTE_Wiki_DIFF)) +
  geom_point(aes(color=WINNER)) +
  scale_color_manual(values=c("blue")) +
  geom_text(aes(label=State_Abv),vjust = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linetype="dotted") +
  geom_vline(xintercept=1,linetype="dashed", color = "gray40") +
  labs(title = "538 Model - Election Results Margins vs 2020 State Elasticity Scores",subtitle = "Biden-Won States",
       x= "2020 State Elasticity Score", y= "Margin Differences (538 Final Model - Wikipedia Results)")

#Graph3
state_data %>%
  filter(WINNER=="TRUMP") %>%
  ggplot(mapping=aes(x=Elasticity_Score, y=FTE_Wiki_DIFF)) +
  geom_point(aes(color=WINNER)) +
  scale_color_manual(values=c("red")) +
  geom_text(aes(label=State_Abv),vjust = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linetype="dotted", color = "red") +
  geom_vline(xintercept=1,linetype="dashed", color = "gray40") +
  labs(title = "538 Model - Election Results Margins vs 2020 State Elasticity Scores",subtitle = "Trump-Won States",
       x= "2020 State Elasticity Score", y= "Margin Differences (538 Final Model - Wikipedia Results)")

#Dem Linear Model
dem_states <- state_data %>%
  filter(WINNER=="BIDEN")
dem_model  <- lm(FTE_Wiki_DIFF ~ Elasticity_Score, data = dem_states)
summary(dem_model)

#Rep Linear Model
rep_states <- state_data %>%
  filter(WINNER=="TRUMP")
rep_model  <- lm(FTE_Wiki_DIFF ~ Elasticity_Score, data = rep_states)
summary(rep_model)

#Full Model
full_model  <- lm(FTE_Wiki_DIFF ~ Elasticity_Score, data = state_data)
summary(full_model)
