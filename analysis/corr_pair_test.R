# Load the cocor package
library(cocor)

df_acc <- read.csv('df_corr_iorp_acc_pivot.csv')

# Extract column names for imagery and perception
imagery_columns <- colnames(df_acc)[grepl("_imagery$", colnames(df_acc))]
perception_columns <- colnames(df_acc)[grepl("_perception$", colnames(df_acc))]

results_df_acc <- data.frame(
  First_corr = character(),
  Second_corr = character(),
  z_value = numeric(),
  p_value = numeric(),
  significance = character(),
  stringsAsFactors = FALSE
)


# Loop through each combination of imagery and perception domains
for (imagery_col in imagery_columns) {
  for (perception_col in perception_columns) {
    # Skip if comparing the same domain
    if (sub("_imagery", "", imagery_col) == sub("_perception", "", perception_col)) next
    
    # Construct the formula dynamically
    formula <- as.formula(paste0("~", imagery_col, " + ", sub("_imagery", "_perception", imagery_col), " | ", imagery_col, " + ", perception_col))
    
    # Perform the comparison using cocor
    res <- cocor(formula, data = df_acc)
    
    # Determine significance level based on p-value
    z_value <- res@pearson1898$statistic
    p_value <- res@pearson1898$p.value
    significance <- ifelse(p_value < 0.001, "***",
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", "")))
    
    # Store the results in the dataframe
    results_df_acc <- rbind(results_df_acc, data.frame(
      First_corr = paste(imagery_col, sub("_imagery", "_perception", imagery_col), sep = ", "),
      Second_corr = paste(imagery_col, perception_col, sep = ", "),
      z_value = z_value,
      p_value = p_value,
      significance = significance,
      stringsAsFactors = FALSE
    ))
  }
}

# Loop through each combination of imagery and perception domains
for (perception_col in perception_columns) {
  for (imagery_col in imagery_columns) {
    # Skip if comparing the same domain
    if (sub("_imagery", "", imagery_col) == sub("_perception", "", perception_col)) next
    
    # Construct the formula dynamically
    formula <- as.formula(paste0("~", perception_col, " + ", sub("_perception", "_imagery", perception_col), 
                                 " | ", perception_col, " + ", imagery_col))
    
    # Perform the comparison using cocor
    res <- cocor(formula, data = df_acc)
    
    # Determine significance level based on p-value
    z_value <- res@pearson1898$statistic
    p_value <- res@pearson1898$p.value
    significance <- ifelse(p_value < 0.001, "***",
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", "")))
    
    # Store the results in the dataframe
    results_df_acc <- rbind(results_df_acc, data.frame(
      First_corr = paste(perception_col, sub("_perception", "_imagery", perception_col), sep = ", "),
      Second_corr = paste(perception_col, imagery_col, sep = ", "),
      z_value = z_value,
      p_value = p_value,
      significance = significance,
      stringsAsFactors = FALSE
    ))
  }
}

# Print the results dataframe
print(results_df_acc)

library(openxlsx)
write.csv(results_df_acc, 'acc_result.csv')



### RT
df_rt <- read.csv('df_corr_iorp_rt_pivot.csv')

# Extract column names for imagery and perception
imagery_columns <- colnames(df_rt)[grepl("_imagery$", colnames(df_rt))]
perception_columns <- colnames(df_rt)[grepl("_perception$", colnames(df_rt))]

results_df_rt <- data.frame(
  First_corr = character(),
  Second_corr = character(),
  z_value = numeric(),
  p_value = numeric(),
  significance = character(),
  stringsAsFactors = FALSE
)


# Loop through each combination of imagery and perception domains
for (imagery_col in imagery_columns) {
  for (perception_col in perception_columns) {
    # Skip if comparing the same domain
    if (sub("_imagery", "", imagery_col) == sub("_perception", "", perception_col)) next
    
    # Construct the formula dynamically
    formula <- as.formula(paste0("~", imagery_col, " + ", sub("_imagery", "_perception", imagery_col), " | ", imagery_col, " + ", perception_col))
    
    # Perform the comparison using cocor
    res <- cocor(formula, data = df_rt)
    
    # Determine significance level based on p-value
    z_value <- res@pearson1898$statistic
    p_value <- res@pearson1898$p.value
    significance <- ifelse(p_value < 0.001, "***",
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", "")))
    
    # Store the results in the dataframe
    results_df_rt <- rbind(results_df_rt, data.frame(
      First_corr = paste(imagery_col, sub("_imagery", "_perception", imagery_col), sep = ", "),
      Second_corr = paste(imagery_col, perception_col, sep = ", "),
      z_value = z_value,
      p_value = p_value,
      significance = significance,
      stringsAsFactors = FALSE
    ))
  }
}

# Loop through each combination of imagery and perception domains
for (perception_col in perception_columns) {
  for (imagery_col in imagery_columns) {
    # Skip if comparing the same domain
    if (sub("_imagery", "", imagery_col) == sub("_perception", "", perception_col)) next
    
    # Construct the formula dynamically
    formula <- as.formula(paste0("~", perception_col, " + ", sub("_perception", "_imagery", perception_col), 
                                 " | ", perception_col, " + ", imagery_col))
    
    # Perform the comparison using cocor
    res <- cocor(formula, data = df_rt)
    
    # Determine significance level based on p-value
    z_value <- res@pearson1898$statistic
    p_value <- res@pearson1898$p.value
    significance <- ifelse(p_value < 0.001, "***",
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", "")))
    
    # Store the results in the dataframe
    results_df_rt <- rbind(results_df_rt, data.frame(
      First_corr = paste(perception_col, sub("_perception", "_imagery", perception_col), sep = ", "),
      Second_corr = paste(perception_col, imagery_col, sep = ", "),
      z_value = z_value,
      p_value = p_value,
      significance = significance,
      stringsAsFactors = FALSE
    ))
  }
}

# Print the results dataframe
print(results_df_rt)

library(openxlsx)
write.csv(results_df_rt, 'rt_result.csv')
