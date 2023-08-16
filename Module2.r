library(readxl)

data <- read_excel("./Data_Asbestos.xlsx", sheet = "Data_196_subjects")

data$Gender <- as.factor(data$Gender)

data$Disease <- as.factor(data$Disease)

data$Socioeconomic_status <- as.factor(data$Socioeconomic_status)

data$Exposure_qual <- as.factor(data$Exposure_qual)

summary(data)
str(data$Disease)

plot(Disease ~ Gender, data = data,
     xlab = "Gender",
     ylab = "Disease")

plot(Disease ~ Socioeconomic_status, data = data,
     xlab = "Socioeconomic Status",
     ylab = "Disease")

plot(Exposure_quant ~ Gender, data = data)

#1 NO, 2 YES
data$Disease <- as.numeric(data$Disease)

#1 Female, 2 Male
data$Gender <- as.numeric(data$Gender)

cat("Correlation disease and gender: \n")
#cor(data$Disease, data$Gender) # 0.3110105
cor.test(data$Disease, data$Gender)

data$socio_num <- as.numeric(data$Socioeconomic_status)

cat("Correlation disease and socioeconomic: \n")
cor.test(data$Disease, data$socio_num)

data$Disease <- ifelse(data$Disease == "2", 1, 0)

print("Non adjusted model")
simple_reg_model <- glm(data$Disease ~ data$Exposure_quant, data = data)
summary(simple_reg_model)

print("Non adjusted model binomial")
simple_reg_model_bin <- glm(data$Disease ~ data$Exposure_quant, data = data, family = binomial(logit))
summary(simple_reg_model_bin)

print("Adjusted model")
multiple_reg_model <- glm(data$Disease ~ data$Exposure_quant + data$Gender + data$Age + data$Socioeconomic_status, data = data)
summary(multiple_reg_model)

print("Adjusted model binomial")
multiple_reg_model_bin <- glm(data$Disease ~ data$Exposure_quant + data$Gender + data$Age + data$Socioeconomic_status, data = data, family = binomial(logit))
summary(multiple_reg_model_bin)

# plot(data$Exposure_quant, data$Disease)
# abline(multiple_reg_model)

cat("\nOdds Ratios (Not Adjusted):\n")
exp(coefficients(simple_reg_model))
cat("\nConfidence Intervals for Odds Ratios (Not Adjusted):\n")
exp(confint.default(simple_reg_model))

cat("\nOdds Ratios (Adjusted):\n")
exp(coefficients(multiple_reg_model))
cat("\nConfidence Intervals for Odds Ratios (Adjusted):\n")
exp(confint.default(multiple_reg_model))

fitted_simple_model <- simple_reg_model_bin$fitted.values
plot(data$Exposure_quant, fitted_simple_model,
     xlab = "Fitted Exposure",
     ylab = "Exposure Quantity")
lines(sort(data$Exposure_quant), fitted_simple_model[order(data$Exposure_quant)],
      col = "#7272ff", lwd = 2)

fitted_multi_model <- multiple_reg_model_bin$fitted.values
plot(data$Exposure_quant, fitted_multi_model,
     xlab = "Fitted Exposure",
     ylab = "Exposure Quantity")
lines(sort(data$Exposure_quant), fitted_simple_model[order(data$Exposure_quant)],
      col = "#7272ff", lwd = 2)


age <- 40
PEL <- 0.1
test_gender <- 2

#Using coefficients from the multiple_reg_model
log_odds <- (-0.398565) + (0.558996 * PEL) + (0.001678 * age) + (0.141479 * test_gender)

odds_ratio <- exp(log_odds)

probability <- odds_ratio / (1 + odds_ratio)

cat("Prediction for 40 year old: ", probability) #0.5018531