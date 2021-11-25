chal_1 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data1.csv")

head(chal_1)

tidied_chal_1 <- chal_1 %>% 
  mutate(Condition = factor(Condition))

tidied_chal_1 %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(RT), sd = sd(RT))

head(tidied_chal_1)

tidied_chal_1 %>% 
  ggplot(aes(x = Condition, y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(alpha = 0.5, width = 0.1) +
  guides(colour = 'none') +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal()

model_1 <- aov_4(RT ~ Condition + (1 | Subject), data = tidied_chal_1)

summary(model_1)

emmeans(model_1, pairwise ~ Condition, adjust = "Bonferroni")


#Challenge 2

chall_2 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data2.csv")

head(chall_2)

chall_2_tidied <- chall_2 %>% 
  mutate(Condition = factor(Condition))

head(chall_2_tidied)

chall_2_tidied %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(RT), sd = sd(RT))

# ggplot(aes(x = fct_reorder(Condition, RT), y = RT, colour = Condition)) +

chall_2_tidied %>% 
  ggplot(aes(x = fct_reorder(Condition, RT), y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(alpha = 0.5, width = 0.1) +
  guides(colour = 'none') +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal()
  
model_2 <- aov_4(RT ~ Condition + (1 | Subject), data = chall_2_tidied)

summary(model_2)

emmeans(model_2, pairwise ~ Condition, adjust = "Bonferroni")

#Challenge 3

chall_3 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data3.csv")

head(chall_3)          

tidied_chal_3 <- chall_3 %>% 
  mutate(Size = factor(Size), Colour = factor(Colour))

head(tidied_chal_3)

tidied_chal_3 %>% 
  group_by(Size, Colour) %>% 
  summarise(mean = mean(RT), sd = sd(RT))

tidied_chal_3 %>% 
  ggplot(aes(x = Size:Colour, y = RT, colour = Size:Colour)) +
  geom_violin() +
  geom_jitter(alpha = 0.5, width = 0.1) +
  guides(colour = 'none') +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal()

model_3 <- aov_4(RT ~ Size * Colour + (1 + Size + Colour | Subject), data = tidied_chal_3)

anova(model_3)

emmeans(model_3, pairwise ~ Size * Colour, adjust = "none")

#Challenge 4

chall_4 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data4.csv")

head(chall_4)

tidied_chal_4 <- chall_4 %>% 
  mutate(Difficulty = factor(Difficulty), Time_Pressure = factor(Time_Pressure), Group = factor(Group))

head(tidied_chal_4)

tidied_chal_4 %>% 
  group_by(Difficulty, Time_Pressure, Group) %>% 
  summarise(mean = mean(RT), sd = sd(RT))

tidied_chal_4 %>% 
  ggplot(aes(x = Difficulty:Time_Pressure, y = RT, colour = Difficulty:Time_Pressure)) +
  geom_violin() +
  geom_jitter(alpha = 0.25, width = 0.1) +
  guides(colour = 'none') +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  facet_wrap(~ Group)

model_4 <- aov_4(RT ~ Difficulty * Time_Pressure * Group + (1 + Difficulty * Time_Pressure | Subject), data = tidied_chal_4)

anova(model_4)

filtered_psych <- tidied_chal_4 %>% 
  filter(Group == "Psychology_Students")

filtered_math <- tidied_chal_4 %>% 
  filter(Group == "Maths_Students")

filtered_art <- tidied_chal_4 %>% 
  filter(Group == "Arts_Students")

psych_model <- aov_4(RT ~ Difficulty * Time_Pressure + (1 + Difficulty * Time_Pressure | Subject), data = filtered_psych)

math_model <- aov_4(RT ~ Difficulty * Time_Pressure + (1 + Difficulty * Time_Pressure | Subject), data = filtered_math)

art_model <- aov_4(RT ~ Difficulty * Time_Pressure + (1 + Difficulty * Time_Pressure | Subject), data = filtered_art)

#Psych is significant
anova(psych_model)

#Maths is NOT significant
anova(math_model)

#Art is NOT significant
anova(art_model)


emmeans(psych_model, pairwise ~ Difficulty * Time_Pressure, adjust = "none")


