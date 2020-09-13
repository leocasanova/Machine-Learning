# PW4

# Leo Casanova

# 05-10-2018


#LOGISTIC REGRESSION

# 1.First, let's do the pre-processing steps you were asked to do during the last session and fit a logistic regression model.

dataset = read.csv(file="Social_Network_Ads.csv")

str(dataset)
summary(dataset)

library(corrplot)
corrplot.mixed(cor(dataset[3:5]))

boxplot(Age ~ Purchased, data=dataset, col = "blue", main="Boxplot Age ~ Purchased")
#We can see that generally speaking, the people that have made the purchase are older.

boxplot(EstimatedSalary ~ Purchased, data=dataset,col = "red", main="Boxplot EstimatedSalary ~ Purchased")
#We can see that generally speaking, the people that have made the purchase have a higher estimated salary.

#anova test
aov(EstimatedSalary ~ Purchased, data=dataset)
summary(aov(EstimatedSalary ~ Purchased, data=dataset))
#the p-value is smaller, so there must be some correlations between 'EstimatedSalary' and 'Purchased'

summary(aov(Age ~ Purchased, data=dataset))
#the p-value is smaller, so there must be some correlations between 'Age' and 'Purchased'

table(dataset$Gender,dataset$Purchased)
mosaicplot(~ Purchased + Gender, data=dataset,main = "MosaicPlot of two categorical variables: Puchased & Gender",color = 2:3, las = 1)
chisq.test(dataset$Purchased, dataset$Gender)

dataset = dataset[3:5]
str(dataset)

library(caTools)
set.seed(211575) 
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

training_set[-3] <- scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])

# logistic regression
classifier.logreg <- glm(Purchased ~ Age + EstimatedSalary , family = binomial, data=training_set)
classifier.logreg
summary(classifier.logreg)

# prediction
pred.glm = predict(classifier.logreg, newdata = test_set[,-3], type="response")

pred.glm_0_1 = ifelse(pred.glm >= 0.5, 1,0)

head(pred.glm)
head(pred.glm_0_1)

# confusion matrix
cm = table(test_set[,3], pred.glm_0_1)
cm

cm = table(pred.glm_0_1, test_set[,3])
cm

mosaicplot(cm,col=sample(1:8,2))

# ROC
require(ROCR)
score <- prediction(pred.glm,test_set[,3])
performance(score,"auc")
plot(performance(score,"tpr","fpr"),col="green")
abline(0,1,lty=8)


#DECISION BOUNDARY OF LOGISTIC REGRESSION

# 2. Plot the decision boundary obtained with logistic regression.

intercept = -coef(classifier.logreg)[1]/(coef(classifier.logreg)[3])
intercept
slope = -coef(classifier.logreg)[2]/(coef(classifier.logreg)[3])
slope
plot(test_set$Age, test_set$EstimatedSalary, xlab = "Age", ylab = "Estimated salary")
title("Estimated salary in function of Age")
abline(intercept, slope)

# 3. In order to verify that your line (decision boundary) is well plotted, color the points on the last Figure with respect to the predicted response.

plot(test_set$Age, test_set$EstimatedSalary, xlab = "Age", ylab = "Estimated salary")
points(test_set[1:2], pch = 21, bg = ifelse(pred.glm_0_1 == 1, "blue", "red"))
abline(intercept, slope, lwd = 2)
title("Decision Boundary of Logistic Regression")


#4. Now make the same plot but color the points with respect to their real labels (the variable Purchased).
# From this figure, count the number of the false positive predictions and compare it to the value obtained in the confusion matrix.

plot(test_set$Age, test_set$EstimatedSalary, xlab = "Age", ylab = "Estimated salary")
points(test_set[1:2], pch = 21, bg = ifelse(test_set[3] == 1, "blue", "red"))
abline(intercept, slope, lwd = 2)
title("Decision Boundary of Logistic Regression")


#LINEAR DISCRIMINANT ANALYSIS (LDA)

# 5. Fit a LDA model of Purchased in function of Age and EstimatedSalary. Name the model classifier.lda.

library(MASS)
classifier.lda <- lda(Purchased~Age+EstimatedSalary, data=training_set)


# 6. Call classifier.lda and see what does it compute.

classifier.lda

classifier.lda$prior
classifier.lda$means


# 7. On the test set, predict the probability of purchasing the product by the users using the model classifier.lda.
# Remark that when we predict using LDA, we obtain a list instead of a matrix, do str() for your predictions to see what do you get.

predict.lda <- predict(classifier.lda, test_set[-3])
predict.lda 
str(predict.lda)


# 8. Compute the confusion matrix and compare the predictions results obtained by LDA to the ones obtained by logistic regression. What do you remark?

confusion.matrix.lda = table(test_set[,3], predict.lda$class)
confusion.matrix.lda

accuracy = sum(diag(confusion.matrix.lda))/sum(confusion.matrix.lda)
accuracy
#The results are not exactly the same when compared to the predictions results obtained by logistic regression but are very close.


# 9. Now let us plot the decision boundary obtained with LDA. 

# create a grid corresponding to the scales of Age and EstimatedSalary
# and fill this grid with lot of points
X1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
# Adapt the variable names
colnames(grid_set) = c('Age', 'EstimatedSalary')

# plot 'Estimated Salary' ~ 'Age'
plot(test_set[, 1:2],
     main = 'Decision Boundary LDA',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

# color the plotted points with their real label (class)
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'red3'))

# Make predictions on the points of the grid, this will take some time
pred_grid = predict(classifier.lda, newdata = grid_set)$class

# Separate the predictions by a contour
contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)


# 10. Now let us build a LDA model for our data set without using the lda() function.

# 10.1 Subset the training set into two sets: class0 where Purchased = 0 and class1 where Purchased = 1).

class0 = subset(training_set, training_set$Purchased == 0)
class1 = subset(training_set, training_set$Purchased == 1)

# 10.2 Compute ??0 and ??1.

pi0 = nrow(class0)/nrow(training_set)
pi0

pi1 = nrow(class1)/nrow(training_set)
pi1

# 10.3 Compute ??0 and ??1.


#QUADRATIC DISCRIMINANT ANALYSIS (QDA)

# 11. Fit a QDA model of Purchased in function of Age and EstimatedSalary. Name the model classifier.qda.

classifier.qda <- qda(Purchased~., data = training_set)
classifier.qda


# 12. Make predictions on the test_set using the QDA model classifier.qda. Show the confusion matrix and compare
# the results with the predictions obtained using the LDA model classifier.lda.

predict.qda <- predict(classifier.qda, test_set[-3])
predict.qda 
str(predict.qda)

confusion.matrix.qda = table(test_set[,3], predict.qda$class)
confusion.matrix.qda 

accuracy = sum(diag(confusion.matrix.qda))/sum(confusion.matrix.qda)
accuracy
#accuracy(lda) = 81% - TP = 23, FP = 6, TN = 58, FN = 13
#accuracy(qda) = 84% - TP = 28, FP = 8, TN = 56, FN = 8
#The QDA has a better accuracy.


# 13. Plot the decision boundary obtained with QDA. Color the points with the real labels.

X1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')

plot(test_set[, 1:2],
     main = 'Decision Boundary QDA',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'red3'))

pred_grid = predict(classifier.qda, newdata = grid_set)$class

contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)


#COMPARISON

# 14. In order to compare the methods we used, plot on the same Figure the ROC curve for each classifier we fitted and compare the correspondant AUC.
# What was the best model for this dataset?

score.glm <- prediction(pred.glm,test_set[,3])
score.lda <- prediction(predict.lda$posterior[,2],test_set[,3])
score.qda <- prediction(predict.qda$posterior[,2],test_set[,3])

performance(score.glm,"auc")
performance(score.lda,"auc")
performance(score.qda,"auc")

plot(performance(score.glm,"tpr","fpr"),col="green")
plot(performance(score.lda,"tpr","fpr"),col="red", add = TRUE)
plot(performance(score.qda,"tpr","fpr"),col="blue", add = TRUE)
abline(0,1,lty=8)

#The blue curve, corresponding to the QDA model, is the best performing : the true positive rate is higher.

