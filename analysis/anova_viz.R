## ANOVA statistics
library(lme4)
library(afex)
library(emmeans)
library(tidyverse)

df = read.csv('df_agg.csv')
df$IorP = factor(df$IorP)
df$field = factor(df$field)
df$participant = factor(df$participant)

df %>% distinct(participant)

# Two way anova: RT
aov.1 <- aov_ez(id = "participant", dv = "ans_rt", data = df, within = c("IorP", "field"))
summary(aov.1)
emmeans(aov.1, pairwise ~ field*IorP)

# Two way anova: Accuracy
aov.2 <- aov_ez(id = "participant", dv = "ans_corr", data = df, within = c("IorP", "field"))
summary(aov.2)
emmeans(aov.2, pairwise ~ field*IorP)


## ANOVA visualization
library(tidyverse)
df_all = read.csv('df_all.csv')
head(df_all)

dim(df_all %>% distinct(participant))

data_rt_summary = df_all %>% 
  group_by(field, IorP) %>%
  summarise(mean=mean(ans_rt), sd=sd(ans_rt), n = n(), sem = sd(ans_corr) / sqrt(n()))

data_rt_summary$field = factor(data_rt_summary$field, levels = unique(data_rt_summary$field))
data_rt_summary <- data_rt_summary[order(data_rt_summary$field),]

data_corr_summary = df_all %>% 
  group_by(field, IorP) %>%
  summarise(mean=mean(ans_corr), sd=sd(ans_corr), n = n(), sem = sd(ans_corr) / sqrt(n()))

data_corr_summary$field = factor(data_corr_summary$field, levels = unique(data_corr_summary$field))

data_corr_summary <- data_corr_summary %>% rename(Modality = IorP, Domain = field)
data_rt_summary <- data_rt_summary %>% rename(Modality = IorP, Domain = field)


ggplot(data_corr_summary, aes(x = Domain, y = mean, color = Modality, group = Modality)) + 
  geom_point(aes(shape = Modality), position = position_dodge(width = 0), alpha=0.4, size = 6) +
  geom_line(aes(linetype = Modality), position = position_dodge(width = 0), size=1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.3, size=1, position = position_dodge(width = 0)) +
  labs(title = "", x = "Domain", y = "Mean Accuracy") +
  theme_bw(base_size = 18) +
  theme(legend.position = c(0.85, 0.21),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  scale_color_brewer(palette = "Dark2")
  

ggplot(data_corr_summary, aes(x = Modality, y = mean, color = Domain, group = Domain)) + 
  geom_point(aes(shape = Domain), position = position_dodge(width = 0), alpha=0.4, size = 6) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  geom_line(aes(linetype = Domain), position = position_dodge(width = 0), size=1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.3, size=1, position = position_dodge(width = 0)) +
  labs(title = "", x = "Modality", y = "Mean Accuracy") +
  theme_bw(base_size = 15) +
  theme(legend.position = c(0.9, 0.3),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  scale_color_brewer(palette = "Dark2")


ggsave("anova_acc.png", width = 4.9, height = 4.2, dpi = 1000)


ggplot(data_rt_summary, aes(x = Domain, y = mean, color = Modality, group = Modality)) + 
  geom_point(aes(shape = Modality), position = position_dodge(width = 0), alpha=0.4, size = 6) +
  geom_line(aes(linetype = Modality), position = position_dodge(width = 0), size=1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.3, size=1, position = position_dodge(width = 0)) +
  labs(title = "", x = "Domain", y = "Mean RT") +
  theme_bw(base_size = 18) +
  theme(legend.position = c(0.6, 0.21),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_color_brewer(palette = "Dark2")

ggplot(data_rt_summary, aes(x = Modality, y = mean, color = Domain, group = Domain)) + 
  geom_point(aes(shape = Domain), position = position_dodge(width = 0), alpha=0.4, size = 6) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  geom_line(aes(linetype = Domain), position = position_dodge(width = 0), size=1) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.3, size=1, position = position_dodge(width = 0)) +
  labs(title = "", x = "Modality", y = "Mean RT") +
  theme_bw(base_size = 15) +
  theme(legend.position = c(0.9, 0.3),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_color_brewer(palette = "Dark2")

ggsave("anova_rt.png", width = 4.9, height = 4.2, dpi = 1000)

