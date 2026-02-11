# ---------------------------------------------------------
# STEP 1: SETUP
# ---------------------------------------------------------
library(tidyverse)

# Load the data
df <- read_csv(file.choose())

# ---------------------------------------------------------
# STEP 2: INSPECTION
# ---------------------------------------------------------
# View structure and summary (Summary shows NAs at the bottom of each column)
str(df)
summary(df)

# ---------------------------------------------------------
# STEP 3: DATA QUALITY AUDIT (FINDING MISSING VALUES)
# ---------------------------------------------------------
# 1. Check total number of missing values
sum(is.na(df))

# 2. Check total rows before cleaning
nrow(df)

# ---------------------------------------------------------
# STEP 4: CLEANING (FIXING MISSING VALUES)
# ---------------------------------------------------------
# In People Analytics, we don't delete everything. 
# We only remove rows where the most important info is missing.

# 1. Remove rows where Employee ID is missing (This is a "Hard" fix)
df_clean <- drop_na(df, employee.id)

# 2. Check rows again to see if any were removed
nrow(df_clean)

# 3. Handle specific categories by converting to Factors
# This ensures R treats text groups correctly even if some have NAs
df_clean$region    <- as.factor(df_clean$region)
df_clean$site.type <- as.factor(df_clean$site.type)
df_clean$role      <- as.factor(df_clean$role)

# ---------------------------------------------------------
# STEP 5: FINAL VERIFICATION
# ---------------------------------------------------------
# Check that the data is ready
str(df_clean)


# ---------------------------------------------------------
# STEP 6: DESCRIPTIVE ANALYSIS (COMPANY TOTALS)
# ---------------------------------------------------------

# Find the "Company Average" by averaging all the "Site Averages"
mean(df_clean$avg.engagement, na.rm = TRUE)

# Find the "Company Average Sales" across all locations
mean(df_clean$avg.monthly.sales.by.site, na.rm = TRUE)

# Find the "Company Average Turnover"
mean(df_clean$annualized.turnover.percent.by.site.number, na.rm = TRUE)


# ---------------------------------------------------------
# STEP 7: GROUPED ANALYSIS (COMPARING REGIONS)
# ---------------------------------------------------------

# 1. Compare Average Engagement by Region
aggregate(avg.engagement ~ region, data = df_clean, mean)

# 2. Compare Average Sales by Region
# We add na.rm = TRUE inside the list to handle the missing HQ values
aggregate(avg.monthly.sales.by.site ~ region, data = df_clean, mean, na.rm = TRUE)

# 3. Compare Turnover Rate by Region
aggregate(annualized.turnover.percent.by.site.number ~ region, data = df_clean, mean)

# 4. Compare Engagement by Site Type (Store vs. Headquarters)
aggregate(avg.engagement ~ site.type, data = df_clean, mean)


# ---------------------------------------------------------
# STEP 8: CHECKING THE LINKS (CORRELATION)
# ---------------------------------------------------------

# 1. Does Engagement link to Sales? 
# (Result near 1.0 means 'Higher Engagement = Higher Sales')
cor(df_clean$avg.engagement, df_clean$avg.monthly.sales.by.site, use = "complete.obs")

# 2. Does Engagement link to Turnover? 
# (Result near -1.0 means 'Higher Engagement = Lower Turnover')
cor(df_clean$avg.engagement, df_clean$annualized.turnover.percent.by.site.number, use = "complete.obs")

# ---------------------------------------------------------
# STEP 9: VISUALIZING THE LINK (SCATTERPLOT)
# ---------------------------------------------------------

# This creates a simple "Dot Map"
# X-axis (bottom) is Engagement, Y-axis (side) is Sales
plot(df_clean$avg.engagement, df_clean$avg.monthly.sales.by.site,
     main = "Happy Employees Sell More",
     xlab = "Engagement Score",
     ylab = "Monthly Sales")

# 2. Add the Red Line
# This tells R: "Draw a line that fits these dots"
abline(lm(avg.monthly.sales.by.site ~ avg.engagement, data = df_clean), col = "red", lwd = 2)

# ---------------------------------------------------------
# STEP 10: EXPORTING DATA AND CHART
# ---------------------------------------------------------

# 1. Save your cleaned data as a new Excel-ready CSV file
write.csv(df_clean, "Cleaned_Employee_Data.csv", row.names = FALSE)

# 2. Save your chart as a high-quality image
dev.copy(png, "Engagement_vs_Sales_Chart.png")
dev.off()

# ---------------------------------------------------------
# STEP 11: PREDICTIVE ANALYSIS (REGRESSION)
# ---------------------------------------------------------

# Predict Sales based on Engagement
model <- lm(avg.monthly.sales.by.site ~ avg.engagement, data = df_clean)

# See the results
summary(model)

# ---------------------------------------------------------
# STEP 11a: MODEL VALIDATION (RESIDUAL PLOT)
# ---------------------------------------------------------

# 1. Create the plot to check for "Random Errors"
plot(model$residuals ~ model$fitted.values, 
     main="Residual Plot: Model Accuracy",
     xlab="Predicted Sales", ylab="Errors (Residuals)",
     pch=16, col="steelblue")

# 2. Add a horizontal line at zero
abline(h = 0, col = "red", lwd = 2)

# 3. Save the validation plot
dev.copy(png, "Model_Validation_Residuals.png")
dev.off()
