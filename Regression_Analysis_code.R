
#Project 3: Regression Analysis#

getwd()


library(readr)
library(ggplot2)
library(olsrr)
library(broom)
library(ggpubr)
library(DescTools) #descriptive sta
library(car)   
library(psych)
library(knitr) #knitr latex table
library(MASS) #stepwise AIC #
library(xtable)


## Data Preparation
bikedata <- read_csv("Bikedata.csv", TRUE)
bikedata <- as.data.frame(bikedata)
head(bikedata)
str(bikedata)

#Setting categorical values
bikedata$Seasons <- factor(bikedata$Seasons)
bikedata$Holiday <- factor(bikedata$Holiday)
str(bikedata)

# Check null entries (missing data)
sum(is.na(bikedata))


## Task 1: Descriptive analysis
#Briefly describe the relationship of the data using descriptive analysis.
describe(bikedata) 

summary(bikedata)

kable(describe(bikedata), format = "latex")



#(i.e. scatter plot)
hourPlt <- ggplot(bikedata,
                  aes(x = Hour , y = log.Rented.Bike.Count)) +
  geom_point() +
  xlab("Hour(Hour of the day)") +
  ylab("Rented bike count (log)")

  

temperaturePlt <- ggplot(bikedata,
                         aes(x = Temperature , y = log.Rented.Bike.Count)) +
  geom_point() +
  xlab(paste("Temperature(\u00B0C)")) +
  ylab("Rented bike count (log)")
 

humidityPlt <- ggplot(bikedata,
                      aes(x = Humidity , y = log.Rented.Bike.Count)) +
  geom_point() +
  xlab("Humidity(%)") +
  ylab("Rented bike count (log)")



windSpeedPlt <- ggplot(bikedata,
                       aes(x = Wind.speed , y = log.Rented.Bike.Count)) +
  geom_point() +
  xlab("Windspeed(m/s)") +
  ylab("Rented bike count (log)")
 


VisibilityPlt <- ggplot(bikedata,
                        aes(x = Visibility , y = log.Rented.Bike.Count)) +
  geom_point() +
  xlab("Visibility(m)") +
  ylab("Rented bike count (log)")



solarRadiationPlt <- ggplot(bikedata,
                            aes(x = Solar.Radiation , y = log.Rented.Bike.Count)) +
  geom_point() +
  xlab(paste("Solar radiation (MJ/m", "\u00B2", ")")) +
  ylab("Rented bike count (log)")



rainfallPlt <- ggplot(bikedata,
                      aes(x = Rainfall , y = log.Rented.Bike.Count)) +
  geom_point() +
  xlab("Rainfall(mm)") +
  ylab("Rented bike count (log)")
 
  

snowfallPlt <- ggplot(bikedata,
                      aes(x = Snowfall , y = log.Rented.Bike.Count)) +
  geom_point() +
  xlab("Snowfall(cm)") +
  ylab("Rented bike count (log)")
 

# Scatter Plot of log.Rented.Bike.Count vs all covariates
figure <- ggarrange(hourPlt, 
                    temperaturePlt, 
                    humidityPlt, 
                    windSpeedPlt, 
                    VisibilityPlt, 
                    solarRadiationPlt, 
                    rainfallPlt, 
                    snowfallPlt, 
                    ncol = 4, 
                    nrow = 2)
figure



#boxplot for categorical variables
seasons <-ggplot(bikedata, aes(x=Seasons, y=log.Rented.Bike.Count, fill, fill=Seasons)) +
  geom_boxplot()



holiday <-ggplot(bikedata, aes(x=Holiday, y=log.Rented.Bike.Count, fill, fill=Holiday)) +
  geom_boxplot()



# extract the median
season_median <- aggregate(log.Rented.Bike.Count ~ Seasons, data = bikedata, FUN = median)
kable(season_median, format = "latex")


holiday_median <- aggregate(log.Rented.Bike.Count ~ Holiday, data = bikedata, FUN = median)
kable(holiday_median , format = "latex")



# Calculate the correlation coefficients
correlation_matrix <- cor(bikedata[, c("log.Rented.Bike.Count", "Hour", "Temperature", "Humidity", "Wind.speed", "Visibility", "Solar.Radiation", "Rainfall", "Snowfall")])

# Extract the correlation coefficients of the response variable
response_correlation <- correlation_matrix[1, -1]

# Create a table of correlation coefficients
correlation_table <- data.frame(Covariate = names(response_correlation), 
                                Correlation_Coefficient = response_correlation,
                                stringsAsFactors = FALSE)

# Print the correlation coefficient table
print(correlation_table)
kable(correlation_table, format = "latex")



## Task 2: Determine a linear regression model of the log.Rented.Bike.Count 
#based on all other given variables.

fit_model2 <-
  lm(log.Rented.Bike.Count ~ Hour + Temperature + Humidity + Wind.speed + Visibility + Solar.Radiation + Rainfall + Snowfall + Seasons + Holiday,
     data = bikedata)

summary(fit_model2)

# Generate the summary of the linear regression model
model_summary <- summary(fit_model2)

# Extract the coefficients table from the model summary
coefficients_table <- model_summary$coefficients

# Display the coefficients table using kable
kable(coefficients_table, format = "latex")




## Task 3: Find a suitable subset of explanatory variables for the 
#log.Rented.Bike.Count. Summarize the regression results, including parameter
#estimates, statistical significance (p-values), confidence intervals, and 
#a goodness-of-fit measure in a table.


########### using stepwise AIC selection method (forward & backward in both direction) ######
# Perform stepwise regression
stepReg <- MASS::stepAIC(fit_model2, direction = "both")

#Analysis of Deviance Table
stepReg$anova
kable(stepReg$anova, format = "latex")


# Summary of stepwise regression
summary(stepReg)


# Summary of stepwise regression into table
summary_table <- xtable::xtable(summary(stepReg))

# ANOVA table of the stepwise regression
anova_table <- xtable::xtable(anova(stepReg))

# Print the summary table in LaTeX format
print(summary_table, type = "latex")

# Print the ANOVA table in LaTeX format
print(anova_table, type = "latex")




# Obtain confidence intervals for estimated regression coefficients
coeff_intervals <- confint(stepReg)

# Print the confidence intervals
print(coeff_intervals)
kable(coeff_intervals, format = "latex")

# Obtain goodness of fit measures
goodness_of_fit <- glance(stepReg)

# Print the goodness of fit measures
print(goodness_of_fit)
kable(goodness_of_fit, format = "latex")





## Task 4: Using the selected model from task 3, create residual plots for 
#model evaluation. Analyze these plots to check for patterns of linearity, 
#heteroskedasticity, and normality. 

### Residual plot and model diagnostics

# check multicollinearity for the full model
car::vif(fit_model2)

# Removing columns with multicollinearity and stepwise
final_model <- lm(log.Rented.Bike.Count ~ Hour + Temperature + Humidity + Wind.speed  + 
                    Rainfall + Seasons + Holiday, data = bikedata)

#summary
summary(final_model)


# Removing columns with multicollinearity and stepwise
car::vif(final_model)

# alternative method: its gives the same value
vif_values <- vif(stepReg)
print(vif_values)

kable(vif_values, format = "latex")


#plot the residual plot
par(mfrow = c(2, 2))
par(mar = c(4, 4, 2, 2))  # Adjust the margins 

plot(final_model, which = 1:4)









