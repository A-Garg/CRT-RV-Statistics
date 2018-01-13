####################################################
# Regression Analysis of RVEF improvement with CRT #
# Created 2018-01-01 by Akhil Garg                 #
# Contact: garga4@vcu.edu                          #
####################################################


# Load data
CRT_data <- read.csv("Regression Data Cleaned - Akhil.csv", na.strings = c("-", "N/A"))

# Save difference in RV function as dependent variable
y <- CRT_data$ï..Diff_CRT_RVEF

# BMI needs to be converted to decimal
CRT_data$BMI <- as.numeric(levels(CRT_data$BMI)[CRT_data$BMI])


# Test response variable for normality using qq plot
plot(density(y))
qqnorm(y)
qqline(y, col = 2)

# For comparison, a QQ plot of baseline LVEF with heavy tails
plot(density(CRT_data$LVEF))
qqnorm(CRT_data$LVEF)
qqline(CRT_data$LVEF, col = 2)
hist(CRT_data$LVEF)

# See distribution of RVEF differences
hist(y)



### Correlation coefficients ###


# Plot correlations and line of best fit
plot(y ~ CRT_data$Systolic)
abline(lm(y  ~ CRT_data$Systolic), col="red")

# Calculate mean of categorical variables (to visually inspect correlation)
mean(CRT_data[CRT_data$LBBB == 0, "ï..Diff_CRT_RVEF"], na.rm = TRUE)
mean(CRT_data[CRT_data$LBBB == 1, "ï..Diff_CRT_RVEF"], na.rm = TRUE)


# Compute correlation coefficients between each independent variable and y
# Use pairwise complete obs deletes missing values
num_columns = c("BMI", "DM", "Dyslip",
                "NYHA", "P.H.Varrh", "P.H.Aaarh",
                "Systolic", "Diastolic", "HR",
                "QRS", "LBBB", "RBBB", "AF",
                "MR.Pre.", "TR.pre.", "LVEF",
                "Na", "K", "Creatine", "Hb")
correlations <- cor(CRT_data[, num_columns], y, method = "spearman", use = "pairwise.complete.obs")
colnames(correlations) <- "rho"
# Sort by descending order, by absolute value
correlations_sorted <- correlations[order(-abs(correlations)),]



### Simple linear regression ###


simple_linear_regression <- function(data_column)
{
  simple_regression <- lm(y ~ data_column)
  print(summary(simple_regression))
  plot(resid(simple_regression)) # plot residuals
  abline(0,0)
}

# Variables from the literature
# All ended up being insignificant
simple_linear_regression(CRT_data$LVEF)
simple_linear_regression(CRT_data$QRS)
simple_linear_regression(CRT_data$LBBB)
simple_linear_regression(CRT_data$NYHA)

# Variables with high correlation coefficients
simple_linear_regression(CRT_data$AF) #barely significant coefficient
simple_linear_regression(CRT_data$DM)
simple_linear_regression(CRT_data$HR)
simple_linear_regression(CRT_data$Systolic) #significant coefficient
simple_linear_regression(CRT_data$P.H.Aaarh)
simple_linear_regression(CRT_data$RBBB)



### Multiple linear regression ###


# Model with factors known to be correlated with success
# See  http://doi.org/10.1111/j.1540-8159.2009.02505.x

# Known/possible factors, for which we have data
### LVEF
### Longer QRS
### LBBB
### NYHA class
# However, in our data, the correlation coefficients for these variables are quite small
# producing an insignificant result
fit_literature <- lm(y ~ LVEF + QRS + LBBB + NYHA, data = CRT_data)
summary(fit_literature)
plot(resid(fit_literature)) # plot residuals
abline(0,0)

# Model with relatively high correlation coefficients
fit_high_coefs <- lm(ï..Diff_CRT_RVEF ~ AF + DM, data = CRT_data)
summary(fit_high_coefs)
plot(resid(fit_high_coefs)) # plot residuals
abline(0,0)

# Model with only one variable
fit_AF <- lm(y ~ CRT_data$AF)
summary(fit_AF)
plot(resid(fit_AF)) # plot residuals
abline(0,0)

# Model with all factors
# (For demonstration purpose only, this model would be overfit)
fit_all <- lm(y ~ ., data = CRT_data)
summary(fit_all)
plot(resid(fit_all)) # plot residuals
abline(0,0)




