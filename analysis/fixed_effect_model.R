library(tidyverse)
library(lmer)
library(lmerTest)
library(ggsci)
library(see)
library(cowplot)

df_all = read.csv('df_all.csv')
head(df_all)

### 1. Confidence on perception performance
df_all_perception = df_all %>% filter(IorP == 'Perception')
df_all_perception_plot <- df_all_perception

# Fixed effect model on RT
fix.effect.rt <- lmer(ans_rt ~ confidence_key + (1 | participant), data = df_all_perception)
summary(fix.effect.rt)
anova(fix.effect.rt)

# Fixed effect model on Accuracy (GLM)
fix.effect.acc <- glmer(ans_corr ~ confidence_key +
                          (1 | participant), data = df_all_perception, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 10)
summary(fix.effect.acc)
anova(fix.effect.acc)

# Condifional res plot for RT
df_all_rt_plot <- df_all_perception

df_all_rt_plot <- df_all_rt_plot %>% 
  mutate(fit.m = predict(fix.effect.rt, re.form = NA),
         fit.c = predict(fix.effect.rt, re.form = NULL))

df_all_rt_plot <- df_all_rt_plot %>%
  mutate(resid = resid(fix.effect.rt))

Cond_DF <- as.data.frame(ranef(fix.effect.rt)) %>% 
  transmute(participant_id = grp, b0_hat = condval) %>% 
  mutate(Intercept_cond = b0_hat + summary(fix.effect.rt)$coef[1,1])

jitter <- position_jitter(width = 0.2, height = 0.5)



df_all_rt_plot %>%
  ggplot(aes(x = confidence_key, y = fit.m + resid)) +
  geom_point(col=12, pch = 16, position = jitter, alpha = 0.2, show.legend=FALSE, size=2) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  geom_line(aes(y = fit.m), col = 1, size = 1) +
  coord_cartesian(ylim = c(-1, 20), xlim = c(0.7, 4.5)) +
  theme_classic(base_size = 18)+
  theme(axis.text=element_text(size=18))+
  labs(y = "Model-Adjusted RT", x = 'Trial-by-trial Confidence')
ggsave("rt_conf.png", width = 4.8, height = 4.1, dpi = 1000)



# Plot for accuracy
# Extract the fixed effect coefficients
fix_eff <- fixef(fix.effect.acc)

# The variable of interest is confidence_key, let's assume it's numeric for simplicity. 
# If it's a factor, you'll have to adjust the code accordingly.

# Create a new data frame for plotting
vividness_range <- c(1, 2, 3, 4)
pred_data <- data.frame(confidence_key = vividness_range)

# Use the fixed effects to predict
pred_data$prob <- plogis(fix_eff[1] + fix_eff[2] * pred_data$confidence_key)  # Convert log-odds to probabilities

# Plot
jitter_acc<- position_jitter(width = 0.2, height = 0.05)
ggplot(pred_data, aes(x = confidence_key, y = prob)) +
  geom_point(data = df_all_perception_plot, aes(x = confidence_key, y = ans_corr, col=participant_id),
             position = jitter_acc, alpha = 0.1, show.legend = FALSE)  +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  geom_line(linewidth = 1) +  # Plot the predicted probabilities +
  labs(x = "Vividness Key", y = "Probability of Correct Answer") +  # Label the axes
  theme_classic() 


# Assuming 'rand_eff' contains the random effects extracted from your mixed model
# and 'fix.effect.acc' is your mixed model object.
# You would typically extract random effects like this:
rand_eff <- ranef(fix.effect.acc)$participant

# Adjust the data frame for plotting individual points (if you have random effects)
# This is simplified and would need to be adapted to your specific model structure
df_all_perception_plot$adjusted_prob <- NA  # Initialize a column for adjusted probabilities

for(i in 1:nrow(df_all_perception_plot)) {
  participant_effect <- rand_eff[[df_all_perception_plot$participant[i],1]]
  df_all_perception_plot$adjusted_prob[i] <- plogis(fix_eff[1] + fix_eff[2] * df_all_perception_plot$confidence_key[i] + participant_effect)
}

# Now, plot using the adjusted probabilities
ggplot(pred_data, aes(x = confidence_key, y = prob)) +
  geom_point(col=12, pch = 16, data = df_all_perception_plot, aes(x = confidence_key, y = adjusted_prob, col = participant_id),
             position = jitter_acc, alpha = 0.2, show.legend = FALSE, size=2) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  geom_line(aes(x = confidence_key, y = prob), size = 1) +  # Plot the fixed effect predictions
  theme_classic(base_size = 18) +
  theme(axis.text=element_text(size=18),
        axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),) +
  labs(x = "Trial-by-trial Confidence", y = "Probability of\n Correct Response")

ggsave("acc_conf.png", width = 5.2, height = 4.2, dpi = 1000)


### 2. Vividness on imagery performance
# Only the imagery tasks
df_all_imagery = df_all %>% filter(IorP == 'Imagery')
df_all_imagery_plot <- df_all_imagery

# Fixed effect model on RT
fix.effect.rt <- lmer(ans_rt ~ vividness_key + (1 | participant), data = df_all_imagery)
summary(fix.effect.rt)
anova(fix.effect.rt)

# # Cut off values 
# cutoff_points <- c(20, 10, 7)
# models <- list()

# # Loop over each cutoff point
# for(cutoff in cutoff_points) {
#   # Filter data based on the current cutoff point
#   filtered_data <- subset(df_all_imagery, ans_rt <= cutoff)
  
#   # Fit fixed effect model
#   model_name <- paste("fix.effect.rt", cutoff, sep = "_")
#   models[[model_name]] <- lmer(ans_rt ~ vividness_key + (1 | participant), data = filtered_data)
  
#   # Print summary of the fitted model
#   cat("Summary for cutoff point", cutoff, ":\n")
#   print(summary(models[[model_name]]))
#   cat("\n\n") # Just to add some space between outputs
# }


# Fixed effect model on Accuracy (GLM)
fix.effect.acc <- glmer(ans_corr ~ vividness_key +
                        (1 | participant), data = df_all_imagery, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 10)
summary(fix.effect.acc)
anova(fix.effect.acc)


# Conditional res plot for RT
df_all_rt_plot <- df_all_imagery

df_all_rt_plot <- df_all_rt_plot %>% 
  mutate(fit.m = predict(fix.effect.rt, re.form = NA),
         fit.c = predict(fix.effect.rt, re.form = NULL))

df_all_rt_plot <- df_all_rt_plot %>%
  mutate(resid = resid(fix.effect.rt))

Cond_DF <- as.data.frame(ranef(fix.effect.rt)) %>% 
  transmute(participant_id = grp, b0_hat = condval) %>% 
  mutate(Intercept_cond = b0_hat + summary(fix.effect.rt)$coef[1,1])

jitter <- position_jitter(width = 0.2, height = 0.5)

df_all_rt_plot %>%
  ggplot(aes(x = vividness_key, y = fit.m + resid)) +
  geom_point(col=12, pch = 16, position = jitter, alpha = 0.2, show.legend=FALSE, size=2) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  geom_line(aes(y = fit.m), col = 1, size = 1) +
  coord_cartesian(ylim = c(-1, 20), xlim = c(0.7, 4.5)) +
  theme_classic(base_size = 18)+
  theme(axis.text=element_text(size=18))+
  labs(y = "Model-Adjusted RT", x = 'Trial-by-trial Vividness')
ggsave("rt_vivid.png", width = 4.8, height = 4.1, dpi = 1000)



# Plot for accuracy
# Extract the fixed effect coefficients
fix_eff <- fixef(fix.effect.acc)

# The variable of interest is vividness_key, let's assume it's numeric for simplicity. 
# If it's a factor, you'll have to adjust the code accordingly.

# Create a new data frame for plotting
vividness_range <- c(1, 2, 3, 4)
pred_data <- data.frame(vividness_key = vividness_range)

# Use the fixed effects to predict
pred_data$prob <- plogis(fix_eff[1] + fix_eff[2] * pred_data$vividness_key)  # Convert log-odds to probabilities


# Assuming 'rand_eff' contains the random effects extracted from your mixed model
# and 'fix.effect.acc' is your mixed model object.
# You would typically extract random effects like this:
rand_eff <- ranef(fix.effect.acc)$participant

# Adjust the data frame for plotting individual points (if you have random effects)
# This is simplified and would need to be adapted to your specific model structure
df_all_imagery_plot$adjusted_prob <- NA  # Initialize a column for adjusted probabilities

for(i in 1:nrow(df_all_imagery_plot)) {
  participant_effect <- rand_eff[[df_all_imagery_plot$participant[i],1]]
  df_all_imagery_plot$adjusted_prob[i] <- plogis(fix_eff[1] + fix_eff[2] * df_all_imagery_plot$vividness_key[i] + participant_effect)
}

# Now, plot using the adjusted probabilities
ggplot(pred_data, aes(x = vividness_key, y = prob)) +
  geom_point(col=12, pch = 16, data = df_all_imagery_plot, aes(x = vividness_key, y = adjusted_prob, col = participant_id),
             position = jitter_acc, alpha = 0.2, show.legend = FALSE, size=2) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  geom_line(aes(x = vividness_key, y = prob), size = 1) +  # Plot the fixed effect predictions
  theme_classic(base_size = 18) +
  theme(axis.text=element_text(size=18),
        axis.line.x.bottom = element_line(color = 'black'),
        axis.line.y.left   = element_line(color = 'black'),) +
  labs(x = "Trial-by-trial Vividness", y = "Probability of\n Correct Response")
  
ggsave("acc_vivid.png", width = 5.2, height = 4.2, dpi = 1000)

## Domain Specific fixed effect
field_lst <- distinct(df_all_imagery, field) %>% pull(field)  # Extracting unique fields as a vector

# RT
for (i in field_lst) {
  print(i)
  df_domain_imagery <- filter(df_all_imagery, field == i)
  fix.effect.rt <- lmer(ans_rt ~ vividness_key + (1 | participant), data = df_domain_imagery)
  print(summary(fix.effect.rt))
}

# Accuracy
for (i in field_lst) {
  print(i)
  df_domain_imagery <- filter(df_all_imagery, field == i)
  fix.effect.acc <- glmer(ans_corr ~ vividness_key +
                            (1 | participant), data = df_all_imagery, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 10)
  print(summary(fix.effect.acc))
}

