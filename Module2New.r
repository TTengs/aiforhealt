library(readxl)

data <- read_excel("./Data_Asbestos.xlsx", sheet = "Data_196_subjects")


#Assignment 1 - demographic
data$Disease_binary <- ifelse(data$Disease == "YES", 1, 0)

model_gender <- glm(Disease_binary ~ Gender, data=data, family=binomial)
summary(model_gender)

model_age <- glm(Disease_binary ~ Age, data=data, family=binomial)
summary(model_age)

model_ses <- glm(Disease_binary ~ Socioeconomic_status, data=data, family=binomial(logit))
summary(model_ses)

# Load the required library
library(ggplot2)

# Create a bar plot to compare gender distribution by disease status
ggplot(data, aes(x=Gender, fill=Disease)) +
  geom_bar(position="dodge") +
  labs(title="Gender Distribution by Disease Status", x="Gender", y="Frequency") +
  scale_fill_manual(values=c("NO"="blue", "YES"="red")) +
  theme_minimal()

# Create a box plot to compare age distribution by disease status
ggplot(data, aes(x=Disease, y=Age, fill=Disease)) +
  geom_boxplot() +
  labs(title="Age Distribution by Disease Status", x="Disease", y="Age") +
  scale_fill_manual(values=c("NO"="blue", "YES"="red")) +
  theme_minimal()

# Assignment 2 - crude and adjusted model and odds ratio
# Unadjusted (Crude) Model
crude_model <- glm(Disease_binary ~ Exposure_quant, data=data, family=binomial(logit))
summary(crude_model)

# Adjusted Model
adjusted_model <- glm(Disease_binary ~ Exposure_quant + Gender + Age + Socioeconomic_status, data=data, family=binomial(logit))
summary(adjusted_model)

cat("\nOdds Ratios (Not Adjusted):\n")
exp(coefficients(crude_model))
cat("\nConfidence Intervals for Odds Ratios (Not Adjusted):\n")
exp(confint.default(crude_model))

cat("\nOdds Ratios (Adjusted):\n")
exp(coefficients(adjusted_model))
cat("\nConfidence Intervals for Odds Ratios (Adjusted):\n")
exp(confint.default(adjusted_model))

# Assignment 4 - Plot the models
# Predicted probabilities
data$predicted_crude <- predict(crude_model, type="response")
data$predicted_adjusted <- predict(adjusted_model, type="response")

# Crude model plot
ggplot(data, aes(x=Exposure_quant, y=predicted_crude)) +
  geom_point(aes(color=Disease_binary)) +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  labs(title="Crude Model: Exposure vs. Predicted Disease Probability", y="Predicted Probability")

# Adjusted model plot
ggplot(data, aes(x=Exposure_quant, y=predicted_adjusted)) +
  geom_point(aes(color=Disease_binary)) +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) +
  labs(title="Adjusted Model: Exposure vs. Predicted Disease Probability", y="Predicted Probability")

PEL <- 0.1  # Permissible exposure limit

# Data frame for the male at different ages and exposure > PEL
new_data <- data.frame(Gender="Male", Age=c(40, 50, 60, 70, 80), 
                       Socioeconomic_status="Middle",  # Assuming middle SES for clarity
                       Exposure_quant=PEL + 0.1)  # Exposure greater than PEL by 0.1 f/cc

new_data$predicted_prob <- predict(adjusted_model, newdata=new_data, type="response")
new_data

ggplot(new_data, aes(x=Age, y=predicted_prob)) +
  geom_line(color="blue", size=1) +
  geom_point(color="red", size=3) +
  labs(title="Predicted Disease Probability Over Age", x="Age", y="Predicted Probability") +
  theme_minimal()


#Assignment 5 - New dataset
# Load the new dataset
data_17_subjects <- read_excel("./Data_Asbestos.xlsx", sheet = "Data_17_subjects")
data_17_subjects_disease <- read_excel("./Data_Asbestos.xlsx", sheet = "Data_17_subjects_Disease")

# Combining the datasets using the correct column name
data_17_subjects$Disease <- data_17_subjects_disease$Disease_outcome
data_17_subjects$Disease_binary <- ifelse(data_17_subjects$Disease == "YES", 1, 0)

# Predict probabilities using the adjusted model
data_17_subjects$predicted_prob <- predict(adjusted_model, newdata=data_17_subjects, type="response")

# Convert probabilities to binary predictions using 0.5 threshold
data_17_subjects$predicted_disease <- ifelse(data_17_subjects$predicted_prob > 0.5, 1, 0)

# Re-compute performance metrics
actual <- data_17_subjects$Disease_binary
predicted <- data_17_subjects$predicted_disease

accuracy <- mean(actual == predicted)
sensitivity <- sum(actual == 1 & predicted == 1) / sum(actual == 1)
specificity <- sum(actual == 0 & predicted == 0) / sum(actual == 0)
precision <- sum(actual == 1 & predicted == 1) / sum(predicted == 1)

list(accuracy=accuracy, sensitivity=sensitivity, specificity=specificity, precision=precision)

# Perform linear regression
linear_model <- lm(predicted_prob ~ Age, data=new_data)

# Print the summary of the linear regression model
summary(linear_model)

# Create a scatter plot with the linear regression line
ggplot(new_data, aes(x=Age, y=predicted_prob)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="blue") +
  labs(title="Linear Regression: Predicted Probability Over Age", x="Age", y="Predicted Probability") +
  theme_minimal()
