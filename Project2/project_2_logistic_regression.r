# Logistical Regression Exercise

library(dplyr)
library(ISLR)
library(car) 
install.packages("pROC")
library(pROC)

raw_data <- read.csv("~/Documents/STAT05/Project2/diabetes.csv")

# Split Data
smp_siz <- floor(0.50*nrow(raw_data))
smp_siz
indices <- sample(seq_len(nrow(raw_data)),size = smp_siz)
test_data <- raw_data[indices,]
train_data <- raw_data[-indices,]

# Apply Logistical Regression
my_model <- glm(Outcome ~ ., family = binomial(link = 'logit'), data = training_data)
View(my_model)
predicted_probs <- predict.glm(my_model, type = "response", newdata=test_data)
View(predicted_probs)

# Create Misclassification Tables
table(actual_outcome = test_data$Outcome, predicted_outcome = predicted_probs>0.5)
table(actual_outcome = test_data$Outcome, predicted_outcome = predicted_probs>0.75)
table(actual_outcome = test_data$Outcome, predicted_outcome = predicted_probs>0.25)
# As predicted probability goes up, the correct false identifications increase, but the alpha value does as well
# As the predicted probability goes down, the correct true identifications increase, but the beta value does as well

# Plot ROC 

g <- roc(Outcome ~ predicted_probs, data = test_data)
plot(g)


#From your model, what is the increase  in the odds of having  diabetes if your BMI goes up by 1?
# An increase of e^0.0864 Probability (from coefficents table)
plot_data <- cbind(test_data, predicted_probs)
plot(plot_data$predicted_probs, plot_data$BMI, xlab="Predicted Probability",
    ylab="BMI" ,main="BMI vs Predicted Probability of Diabetes")
lines(lowess(plot_data$predicted_probs,plot_data$BMI), col="blue") # lowess line (x,y)
my_model[["coefficients"]] 

# What about if your Glucose level increases  by 1?
# An increase of e^0.0302 (from coefficents table)
plot_data <- cbind(test_data, predicted_probs)
plot(plot_data$predicted_probs, plot_data$Glucose, xlab="Predicted Probability",
     ylab="BMI" , main="Glucose levels vs Predicted Probability of Diabetes")
lines(lowess(plot_data$predicted_probs,plot_data$Glucose), col="red") # lowess line (x,y)
my_model[["coefficients"]] 

