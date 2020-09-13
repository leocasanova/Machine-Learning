# PW3

# Leo Casanova

# 28-09-2018


# 1. Download the Social_Network_Ads dataset  and import it into R.

data=read.csv(file="Social_Network_Ads.csv")


# 2. Explore and Describe the dataset (you can use str() and summary() functions,
# you can calculate and visualize the correlations, show some histograms, scatterplots, pie charts, etc..).

str(data)
summary(data)
cor(data$Age, data$EstimatedSalary)


# 3. Now we are going to split the dataset into training set and test set. Last week we did it manually.
# From now on we will split it randomly, you can use this code (after undestanding it of course):

library(caTools)
set.seed(123)
split = sample.split(data$Purchased, SplitRatio = 0.75)
# here we chose the SplitRatio to 75% of the dataset, and 25% for the test set.
training_set = subset(data, split == TRUE)
# we use subset to split the dataset
test_set = subset(data, split == FALSE)


# 4. Scale the input variables in both training set and test set. Do you know what is scaling? Explain it one sentence.

#On centre-reduit pour mettre les valeurs des 2 colonnes a la meme echelle
training_set[c(3,4)]=scale(training_set[c(3,4)])
test_set[c(3,4)]=scale(test_set[c(3,4)])


# 5. Now fit a simple logistic regression model of Purchased in function of Age.

model1 = glm(Purchased ~ Age , family = binomial, data=training_set)
model1
summary(model1)


# 6. As you saw in the Logistic Regression chapter and in the previous question, we choose argument family to be binomial when we use the function glm.
# Explain why!

# We chose family because it is a Bernoulli/binomial trial with exactly 2 possible outcomes "success" or "failure" (= 1 or 0).
# because logistic regression is a generalized linear model
# where the variable to predict is binary
# (so it follows Bernouilli's law, and Bernouilli
# is an exception of binomial with n=1)


# 7. Write in a equation the model you obtained in question 5? (Read the following note concerning this question).

paste("log(p(x)/1-p(x) =", model1$coefficients[1], "+", model1$coefficients[2], "Age") 


# 8. Is the feature Age significant to the model? Justify your answer.

#The p-value (2e-16) is smaller than the feature Age (1.99), so it is significant to the model.


# 9. What is the value of AIC of the model?

# AIC = 2k - 2ln(L^) = 256.11


# 10. Plot Purchased in function of Age and add the curve of the obtained logistic regression model.
# (Hints: One way to do it is to first plot the observations, then use the curve() function with option add=TRUE in order to add the curve to the plot.
# pay attention that the argument "type" of the function predict() must be reponse)

plot(training_set$Age,training_set$Purchased)
curve(predict(model1, data.frame(Age=x), type="response"), add=TRUE)

#library(ggplot2)
#ggplot(training_set, aes(x=Age, y=Purchased)) + geom_point() + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


# 11. Now let us take another feature into account in the model. Fit a logistic regression model of purchasing the product in function of the age of a user
# and its salary.

model2 <- glm(Purchased ~ Age + EstimatedSalary , family = binomial, data=training_set)
model2
summary(model2)

# 12. Are the predictors significant to the new model?

# The p-value for both Age (2.83e-14) and EstimatedSalary (2.03e-09) are smaller than the features so they are significant.


# 13. Do we obtain a better model by adding the estimated salary?

# The AIC for this model is 205.78. As it is smaller than previous model, it is a better one.


# 14. Predictions: On the test set, predict the probability of purchasing the product by the users using the obtained model.

predictions = predict(model2, newdata = test_set[c(3,4)], type = "response")
predictions


# 15. Take a look on your predicted values for the variable Purchased. We predicted the probability that the user will purchase the product right?
# Now in order to compare your results with the real answers, transform the predicted values to 0 or 1 (1 if >0.5).

pred = ifelse(predictions > 0.5, 1,0)
pred


# 16. Now in order to evaluate the model and its predictions, compute the confusion matrix. What do you obtain ?
# (Hint: you can use the table() function).

confusion_matrix = table(pred, test_set[,5])
confusion_matrix


# 17. Calculate the accuracy, specificity, sensitivity and the precision of the model.
# (Note: You can create a function that takes a confusion matrix in input and returns the needed metrics)

accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

specificity = confusion_matrix[2,2]/sum(confusion_matrix[,2])
specificity

sensitivity = confusion_matrix[1,1]/sum(confusion_matrix[,1])
sensitivity

precision = confusion_matrix[1,1]/sum(confusion_matrix[1,])
precision


# 18. Plot the ROC curve and calculate AUC value.
install.packages("ROCR")
library(ROCR)
score <- prediction(pred,test_set[,5])
performance(score,"auc")

plot(performance(score,"tpr","fpr"),col="green")
abline(0,1,lty=8)
