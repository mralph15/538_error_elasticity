###discarded code
#NYT dataset manipulation
all_states <- read.csv("all-state-changes.csv")
all_states_one <- all_states %>%
  group_by(state) %>%
  slice(1)
all_states_one <- all_states_one %>%
  mutate(trump_vote=ifelse(leading_candidate_name=="Trump",leading_candidate_votes,trailing_candidate_votes)) %>%
  mutate(biden_vote=ifelse(leading_candidate_name=="Biden",leading_candidate_votes,trailing_candidate_votes))
all_states_one <- all_states_one %>%
  mutate(NYT_margin=(trump_vote - biden_vote)/(trump_vote + biden_vote)*100)
all_states_one <- all_states_one %>%
  select(state,trump_vote,biden_vote,NYT_margin)
write.csv(all_states_one, file = "all-state-changes-final.csv")

#FTE dataset manipulation
fte_final <- read.csv("presidential_state_toplines_2020.csv")
fte_final <- fte_final %>%
  mutate(trump_vote=(voteshare_inc/100)*state_turnout) %>%
  mutate(biden_vote=(voteshare_chal/100)*state_turnout) %>%
  mutate(FTE_margin=(trump_vote - biden_vote)/(trump_vote + biden_vote)*100)
write.csv(fte_final, file = "fte_final.csv")

#Graph1
state_data %>%
  ggplot(mapping=aes(x=Elasticity_Score, y=FTE_NYT_DIFF, color=WINNER)) +
  geom_point() +
  scale_color_manual(values=c("blue", "red")) +
  geom_text(aes(label=State_Abv),vjust = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linetype="dotted") +
  geom_vline(xintercept=1,linetype="dashed", color = "gray40") +
  labs(title = "Trump-Biden Two-Candidate Margin between FiveThirtyEight and NYT Results vs 2020 State Elasticity Scores",
       x= "State Elasticity Score", y= "Two Candidate Margin Difference (Trump - Biden)")

#Graph2
state_data %>%
  ggplot(mapping=aes(x=Elasticity_Score, y=FTE_NYT_DIFF)) +
  geom_point(aes(color=WINNER)) +
  scale_color_manual(values=c("blue", "red")) +
  geom_text(aes(label=State_Abv),vjust = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype="dashed") +
  geom_vline(xintercept=1,linetype="dashed", color = "gray40") +
  labs(title = "Trump-Biden Two-Candidate Margin between FiveThirtyEight and NYT Results vs 2020 State Elasticity Scores",
       x= "State Elasticity Score", y= "Margin Difference (Trump - Biden)")

model <- lm(FTE_NYT_DIFF ~ Elasticity_Score, data = state_data)
summary(model)

test1 <- state_data %>%
  filter(WINNER=="TRUMP")
model2 <- lm(FTE_NYT_DIFF ~ Elasticity_Score, data = test1)
summary(model2)

test2 <- state_data %>%
  filter(WINNER=="BIDEN")
model3 <- lm(FTE_NYT_DIFF ~ Elasticity_Score, data = test2)
summary(model3)
