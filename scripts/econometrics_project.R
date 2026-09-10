# ==============================================================================
# FULL PROJECT SCRIPT: EDA, WINSORIZATION, & STEPWISE MODELING
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(stringr)


# Read the dataset
print("Please select the 'ds_salaries.csv' file...")
df <- read.csv(file.choose())

# ------------------------------------------------------------------------------
# 2. DATA PRE-PROCESSING & CLEANING
# ------------------------------------------------------------------------------
print("Starting data preprocessing...")

df_clean <- df %>% select(-1, -salary, -salary_currency)

# Factors Transformation
df_clean$experience_level <- factor(df_clean$experience_level, levels = c("EN", "MI", "SE", "EX"))
df_clean$company_size     <- factor(df_clean$company_size, levels = c("S", "M", "L"))
df_clean$remote_ratio      <- factor(df_clean$remote_ratio, levels = c(0, 50, 100), labels = c("OnSite", "Hybrid", "Remote"))
df_clean$work_year         <- as.factor(df_clean$work_year)
df_clean$employment_type   <- as.factor(df_clean$employment_type)

# Simplify Location and Job Titles
df_clean$company_location  <- as.factor(ifelse(df_clean$company_location == "US", "US", "Non-US"))
# ------------------------------------------------------------------------------
# UPDATED: Advanced Job Title Categorization using Pattern Matching
# ------------------------------------------------------------------------------
# Simplify job titles into four main functional categories
df_clean <- df_clean %>%
  mutate(job_title = case_when(
    str_detect(tolower(job_title), "scientist") ~ "Scientist",
    str_detect(tolower(job_title), "engineer")  ~ "Engineer",
    str_detect(tolower(job_title), "analyst")   ~ "Analyst",
    TRUE                                        ~ "Other"
  )) %>%
  # Convert the new categories into a Factor for statistical modeling
  mutate(job_title = as.factor(job_title))

# Verify the new distribution
print(table(df_clean$job_title))
# Set Reference Level for Experience
df_clean$experience_level <- relevel(df_clean$experience_level, ref = "EN")

# ------------------------------------------------------------------------------
# 3. VISUALIZING RELATIONSHIPS (EDA PLOTS)
# ------------------------------------------------------------------------------
print("Generating Plots to visualize relationships...")

#plotting all categorical variables
cat_cols <- df_clean %>% select_if(is.factor) %>% names()

for (col in cat_cols) {
  
  # 1. Plot: Salary Distribution (Boxplot) per variable to detect variance and outliers
  p_box <- ggplot(df_clean, aes(x = !!sym(col), y = salary_in_usd, fill = !!sym(col))) +
    geom_boxplot(outlier.color = "red", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Salary Distribution by", col), y = "Salary (USD)", x = col) +
    theme(legend.position = "none")
  
  # Flip coordinates for better readability if the variable is 'job_title'
  if(col == "job_title") p_box <- p_box + coord_flip()
  print(p_box)
  
  # 2. Plot: Average Salaries (Bar Chart) to compare mean values across categories
  avg_sal_data <- df_clean %>% group_by(!!sym(col)) %>% summarise(mean_salary = mean(salary_in_usd))
  
  p_bar <- ggplot(avg_sal_data, aes(x = reorder(!!sym(col), mean_salary), y = mean_salary, fill = !!sym(col))) +
    geom_col() +
    theme_minimal() +
    labs(title = paste("Average Salary by", col), y = "Mean Salary (USD)", x = col) +
    theme(legend.position = "none")
  
  if(col == "job_title") p_bar <- p_bar + coord_flip()
  print(p_bar)
}

# ------------------------------------------------------------------------------
# 4. MODEL 1: FULL ORIGINAL MODEL
# ------------------------------------------------------------------------------
print("=== Model 1: Full Original Model (Raw Salary) ===")
full_model <- lm(salary_in_usd ~ experience_level + company_size + remote_ratio + 
                   work_year + company_location + job_title, data = df_clean)
print(summary(full_model))

# ------------------------------------------------------------------------------
# 5. TESTING ASSUMPTIONS (For FULL ORIGINAL MODEL)
# ------------------------------------------------------------------------------
print("=== Starting Model Diagnostics ===")

print("--- 1. VIF Test ---")
print(vif(full_model))

print("--- 2. Breusch-Pagan Test ---")
print(bptest(full_model))

print("--- 3. Shapiro-Wilk Test ---")
print(shapiro.test(residuals(full_model)))

print("--- 4. Durbin-Watson Test ---")
print(dwtest(full_model))

par(mfrow = c(2, 2))
plot(full_model, main = "Diagnostics: FULL ORIGINAL MODEL")
par(mfrow = c(1, 1))

print("All processes completed successfully!")

# ------------------------------------------------------------------------------
# 6. WINSORIZATION (Handling Outliers)
# ------------------------------------------------------------------------------
print("Applying Winsorization (Manual Capping)...")

lower_limit <- quantile(df_clean$salary_in_usd, 0.05, na.rm = TRUE)
upper_limit <- quantile(df_clean$salary_in_usd, 0.95, na.rm = TRUE)

df_clean$winsor_salary <- df_clean$salary_in_usd
df_clean$winsor_salary[df_clean$winsor_salary > upper_limit] <- upper_limit
df_clean$winsor_salary[df_clean$winsor_salary < lower_limit] <- lower_limit

# Visual verification of the distribution after capping
boxplot(df_clean$winsor_salary, main = "Salary After Winsorization", col = "lightgreen")

# ------------------------------------------------------------------------------
# 7. MODEL 2: WINSORIZED STEPWISE MODEL
# ------------------------------------------------------------------------------
print("Building Winsorized Full Model...")
full_winsor_model <- lm(winsor_salary ~ experience_level + company_size + remote_ratio + 
                          work_year + company_location + job_title, data = df_clean)

print("Running Stepwise to find the Best Model...")
best_winsor_model <- step(full_winsor_model, direction = "backward", trace = 0)
print(summary(best_winsor_model))

# ------------------------------------------------------------------------------
# 8. TESTING ASSUMPTIONS (For Best Winsorized Model)
# ------------------------------------------------------------------------------
print("=== Starting Model Diagnostics ===")

print("--- 1. VIF Test ---")
print(vif(best_winsor_model))

print("--- 2. Breusch-Pagan Test ---")
print(bptest(best_winsor_model))

print("--- 3. Shapiro-Wilk Test ---")
print(shapiro.test(residuals(best_winsor_model)))

print("--- 4. Durbin-Watson Test ---")
print(dwtest(best_winsor_model))

print("--- 5. Remsy-Reset Test ---")
print(resettest(best_winsor_model))

par(mfrow = c(2, 2))
plot(best_winsor_model, main = "Diagnostics: Best Winsorized Model")
par(mfrow = c(1, 1))

print("All processes completed successfully!")

# ------------------------------------------------------------------------------
# 9. MODEL 2: log WINSORIZED STEPWISE MODEL
# ------------------------------------------------------------------------------

df_clean$log_winsor_salary <- log(df_clean$winsor_salary)

print("Building Winsorized Full Model...")
log_winsor_model <- lm(log_winsor_salary ~ experience_level + company_size + remote_ratio + 
                         work_year + company_location + job_title, data = df_clean)

print("Running Stepwise to find the Best Model...")
best_log_model <- step(log_winsor_model, direction = "backward", trace = 0)
print(summary(best_log_model))

# ------------------------------------------------------------------------------
# 10. TESTING ASSUMPTIONS (For Best Log Winsorized Model)
# ------------------------------------------------------------------------------
print("=== Starting Model Diagnostics ===")

print("--- 1. VIF Test ---")
print(vif(best_log_model))

print("--- 2. Breusch-Pagan Test ---")
print(bptest(best_log_model))

print("--- 3. Shapiro-Wilk Test ---")
print(shapiro.test(residuals(best_log_model)))

print("--- 4. Durbin-Watson Test ---")
print(dwtest(best_log_model))

par(mfrow = c(2, 2))
plot(best_log_model, main = "Diagnostics: Best log Winsorized Model")
par(mfrow = c(1, 1))

print("All processes completed successfully!")
# ------------------------------------------------------------------------------
# 11. QUESTION 2: DO MARKET DYNAMICS DIFFER IN THE US VS. NON-US?
# (Interaction Effects Model)
# ------------------------------------------------------------------------------
print("=== Q2: Exploring Interaction between Location and Experience ===")

# Building the Interaction Model
# We use '*' to see if the impact of experience is amplified in the US
interaction_model <- lm(log_winsor_salary ~ experience_level * company_location + 
                          company_size + job_title, 
                        data = df_clean)

# Display the statistical summary
print(summary(interaction_model))

# ------------------------------------------------------------------------------
# 12. VISUALIZING THE INTERACTION EFFECT
# ------------------------------------------------------------------------------
# Plotting the interaction to see the difference visually
p_interact <- ggplot(df_clean, aes(x = experience_level, y = log_winsor_salary, 
                                   color = company_location, group = company_location)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  theme_minimal() +
  labs(
    title = "Interaction Effect: Experience vs. Location",
    subtitle = "Comparing how experience pays off in US vs. Non-US",
    y = "Average Winsorized Salary",
    x = "Experience Level"
  )

print(p_interact)


# ------------------------------------------------------------------------------
# 13. QUESTION 3: INTERACTION BETWEEN COMPANY SIZE AND EXPERIENCE LEVEL
# ------------------------------------------------------------------------------
print("=== Q3: Interaction between Company Size and Experience Level ===")

# Testing if the financial reward for seniority varies by company size
interaction_model_v2 <- lm(log_winsor_salary ~ experience_level * company_size + 
                             job_title + company_location, 
                           data = df_clean)

# Display the statistical results
print(summary(interaction_model_v2))

# ------------------------------------------------------------------------------
# 14. VISUALIZING THE INTERACTION (EXPERIENCE vs. COMPANY SIZE)
# ------------------------------------------------------------------------------
p_interact_v2 <- ggplot(df_clean, aes(x = experience_level, y = log_winsor_salary, 
                                      color = company_size, group = company_size)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  theme_minimal() +
  labs(
    title = "Interaction Effect: Experience Level vs. Company Size",
    subtitle = "Does seniority pay off more in Large vs. Small companies?",
    y = "Average Winsorized Salary",
    x = "Experience Level (EN, MI, SE, EX)"
  )

print(p_interact_v2)

# ==============================================================================
# 15.ASSUMPTION TESTING: LOCATION & EXPERIENCE INTERACTION
# ==============================================================================
print("=== Diagnostics for Q2: Location * Experience Model ===")

# 1. Multicollinearity (VIF) - Checking if variables are too correlated
# Note: Interaction terms naturally have high VIF, focus on main effects
print("--- 1. VIF Test ---")
print(vif(interaction_model))

# 2. Homoscedasticity (Breusch-Pagan Test)
print("--- 2. Breusch-Pagan Test ---")
print(bptest(interaction_model))

# 3. Normality of Residuals (Shapiro-Wilk)
print("--- 3. Shapiro-Wilk Test ---")
print(shapiro.test(residuals(interaction_model)))

# 4. Independence of Residuals (Durbin-watson)
print("--- 4. Durbin-Watson Test ---")
print(dwtest(interaction_model))

# 5. Visual Diagnostics
par(mfrow = c(2, 2))
plot(interaction_model, main = "Diagnostics: Location * Experience")
par(mfrow = c(1, 1))

# ==============================================================================
# 16.ASSUMPTION TESTING: COMPANY SIZE & EXPERIENCE INTERACTION
# ==============================================================================
print("=== Diagnostics for Q3: Company Size * Experience Model ===")

# 1. Multicollinearity (VIF)
print("--- 1. VIF Test ---")
print(vif(interaction_model_v2))

# 2. Homoscedasticity (Breusch-Pagan Test)
print("--- 2. Breusch-Pagan Test ---")
print(bptest(interaction_model_v2))

# 3. Normality of Residuals (Shapiro-Wilk)
print("--- 3. Shapiro-Wilk Test ---")
print(shapiro.test(residuals(interaction_model_v2)))

# 4. Independence of Residuals (Durbin-watson)
print("--- 4. Durbin-Watson Test ---")
print(dwtest(interaction_model_v2))

# 5. Visual Diagnostics
par(mfrow = c(2, 2))
plot(interaction_model_v2, main = "Diagnostics: Size * Experience")
par(mfrow = c(1, 1))
