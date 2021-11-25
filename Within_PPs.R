rm_data <- read.csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/rm_data.csv")

head(rm_data)

rm_data_tidied <- rm_data %>% 
  mutate(Condition = factor(Condition))

rm_data_tidied %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(RT), sd = sd(RT)) %>% 
  arrange(mean)

rm_data_tidied %>%
  ggplot(aes(x = fct_reorder(Condition, RT), y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Condition", y = "RT (s)")

model <- aov_4(RT ~ Condition + (1 + Condition | Participant), data = rm_data_tidied)

summary(model)

emmeans(model, pairwise ~ Condition, adjust = "Bonferroni")
