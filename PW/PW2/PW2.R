# PW2

# Leo Casanova

# 21-09-2018


# 1. Load the Boston dataset from MASS package.
library(MASS)
dim(Boston)

# 2. Split the dataset into traning set and testing set. (keep all the variables of the Boston data set)
train = 1:400
test = -train

training_set = Boston[train,]
test_set = Boston[test,]

#3. Check if there is a linear relationship between the variables medv and age. (use cor() function).
cor(training_set$medv,training_set$age)

#4. Fit a model of housing prices in function of age and plot the observations and the regression line.
model1 <- lm(medv ~ age , data= training_set)
model1
plot(training_set$age, training_set$medv,
     xlab = "Age of the house",
     ylab = "Median House Value",
     col = "red",
     pch = 20)

abline(model1, col = "blue", lwd =3)

#5. Train a regression model using both lstat and age as predictors of median house value.
# (Remember that we transformed lstat, use the same transformation here). What is the obtained model?
model2 <- lm(formula = medv ~ age + log(lstat), data = training_set)
model2

# what is the obtained model means the coefficients Y = 51.60212 - 14.33897 log(stat) + 0.07779 age

#6. Print the summary of the obtained regression model.
summary(model2)

#7. Are the predictors significant ?
# re-explain the interpretation of the summary table
# looking at the p-values all the predictors here are significant (compare p-value with the error etc..)

#8. Is the model as a whole significant? Answer on this question must be detailed.
# answer to this question in the summary table
# the p-value of the Fisher's test (last line)
# compare p-value with error
# here the model is significant (at least one coefficient non-nul)

#9. Train a new model using all the variables of the dataset. (We can use . as a short cut instead of writing down all the variables names)
model3 <- lm(formula = medv ~ ., data = training_set)
model3
summary(model3)

#10. When using all the variables as predictors, we didn't transform lstat. Re train the model using log(lstat) instead of lstat.
model4 <- lm(formula = medv ~ . - lstat + log(lstat), data = training_set)
model4

#11. Did R² improve ?
summary(model1)$r.squared #R² = 0.08
summary(model2)$r.squared #R² = 0.67
summary(model3)$r.squared #R² = 0.73
summary(model4)$r.squared #R² = 0.78

summary(model4)$r.squared > summary(model3)$r.squared

#12. To see if there is correlated variables print the correlation matrix using the cor() function (round the correlations with 2 digits).
round(cor(training_set), digits = 2) 

#13. Visualize the correlations using the corrplot package. To do so, install the corrplot package, load it, then use the function corrplot.mixed().
#install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(training_set))

#14. What is the correlation between tax and rad?
cor(training_set$tax, training_set$rad) 

#15. Run the model again without tax. What happens to the R²? and for the F-statistic?
model5 = lm(medv ~ . -tax - lstat + log(lstat), data = training_set)
model5
summary(model5)
summary(model5)$r.squared #R² = 0.78
summary(model4)$fstatistic
summary(model5)$fstatistic

#Of course R2 should go a little lower because we deleted one of the variables. But check for the model significance (F-statistic) gets higher,
#which means the p-values gets lower and thus the model is more significant without rad.

#16. Calculate the mean squared error (MSE) for the last model.
y = test_set$medv
y_hat = predict(model5, test_set)
error = y-y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE

#17
str(training_set)
table(training_set$chas)
sum(training_set$chas == 1)

#18
boxplot(medv~chas, data=training_set)

#19
aggregate(formula=medv~chas, data=training_set, FUN=mean)

#20
anovatest <- aov(medv~chas, data= training_set)
anovatest
summary(anovatest)

#21
model21 <- lm(medv~chas+crim,data=training_set)
model21
summary(model21)

#22
model22 <- lm(medv~.,data=training_set)
summary(model22)

#Pr(>|t|) de chas = 0.055
#significant with error 5%, so we are less confident that it is significant than before

#23
model23 <- lm(medv~lstat*age,data=training_set) # or medv~lstat+age+lstat:age
summary(model23)

#24
model24 <- lm(medv~.^2,data=training_set) #just to learn how to do it
summary(model24)
