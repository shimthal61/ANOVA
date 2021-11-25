my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/cond.csv")

my_data_tidied <- my_data %>% 
  mutate(Condition = factor(Condition))

head(my_data_tidied)

my_data_tidied %>%
  group_by(Condition) %>% 
  summarise(mean = mean(Ability), sd = sd(Ability))

set.seed(1234)
my_data_tidied %>% 
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  guides(colour = 'none') +
  geom_jitter(alpha = .5, width = 0.2) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) 

#The correct syntax is aov_4(DV ~ IV + (1 | Participant), data = my_data_tidied)

(model <- aov_4(Ability ~ Condition + (1 | Participant), data = my_data_tidied))

#By default this is set to Tukey
emmeans(model, pairwise ~ Condition)

#We can set this to bonferroni
emmeans(model, pairwise ~ Condition, adjust = "bonferroni")


