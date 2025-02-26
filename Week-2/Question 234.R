

# Given data
Age <- c(25, 56, 65, 32, 41, 49)
Income <- c(49000, 156000, 99000, 192000, 39000, 57000)
# Create a dataframe
data <- data.frame(Age = Age, Income = Income)

data
# Calculate mean
mean_age <- mean(Age)
mean_income <- mean(Income)

# Calculate variance
variance_age <- var(Age)
variance_income <- var(Income)

# Calculate standard deviation
std_dev_age <- sd(Age)
std_dev_income <- sd(Income)


# Calculate covariance
covariance <- cov(Age, Income)

# Calculate correlation
correlation <- cor(Age, Income)

# Display results
cat(paste("Covariance:", covariance, "\n"))
cat(paste("Correlation:", correlation, "\n"))
plot(correlation)

LM_fit<-lm(Income~., data=data)

# Create a scatter plot
plot_ly(data, x = ~Age, y = ~Income, type = 'scatter', mode = "markers", name="Data") %>%
  add_trace(x=~mean(Age), y=~mean(Income), type="scatter", mode="markers",
            name="(mean(Age), mean(Income))", marker=list(size=20, color='blue', line=list(color='yellow', width=2))) %>%
  add_lines(x = ~Age, y = LM_fit$fitted.values, mode = "lines", name="Linear Model") %>%
  layout(title=paste0("lm(Income ~ Age), cor(Age, Income) = ", 
                      round(cor(data$Age, data$Income),3)))

LM_fit


# 2.2 Ordinary Least Squares Estimation
b<-cov(data$Age, data$Income)/var(data$Age)
b
a<-mean(data$Income)-b*mean(data$Age)
a


# First observation
age_observation <- 25

# Calculate fitted value
fitted_value <- a + b * age_observation

# Obtain actual value from the dataset
actual_value <- data$Income[1]

# Calculate residual
residual <- actual_value - fitted_value

# Display results
actual_value  # Actual value for the first observation
fitted_value  # Fitted value for the first observation
residual      # Residual for the first observation



# Obtain actual values
actual_values <- data$Income[1]

# Obtain fitted values
fitted_values <- predict(LM_fit)

# Calculate residuals
residuals <- residuals(LM_fit)

# Display results
results <- data.frame(Actual = actual_values, Fitted = fitted_values, Residual = residuals)
head(results)  # Displaying results for the first few observations


# Standard deviation Reduction:
Hours_Played = c(26, 30, 48, 46, 62, 23, 43, 36, 38, 48, 48, 62, 44, 30)
Outlook <- c("Rainy", "Rainy", "Overcast", "Sunny", "Sunny", "Sunny", "Overcast", "Rainy", "Rainy", "Sunny", "Rainy", "Overcast", "Overcast", "Sunny")
Weather_Data = data.frame(Hours_Played = Hours_Played, Outlook=Outlook)

# Create two attributes a. when outlook is overcast and b .rest are all in another attribute.
# The two attributes indicates the two nodes in our question.
at1 = Hours_Played[Outlook == "Overcast"]
at2 = Hours_Played[Outlook != "Overcast"]

SDR<-sd(Hours_Played)-(length(at1)/length(Hours_Played)*sd(at1)+length(at2)/length(Hours_Played)*sd(at2))
SDR
# predicted mean value of playing hours (outcome variable) for each terminal node of the split.
cat(paste("Mean value for node 1:", mean(at1)))
cat(paste("Mean value for node 2:", mean(at2)))


