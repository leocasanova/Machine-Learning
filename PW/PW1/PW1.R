# PW1

# Leo Casanova

# 07-09-2018


# Ex1

x = c(1, 7, 3, 4)
y = 100:1

x[3] + y[4]
cos(x[3]) + sin(x[2])*exp(-y[2])

x[3] = 0
y[2] = -1

x[3] + y[4]
cos(x[3]) + sin(x[2])*exp(-y[2])

z = y[x+1]


# Ex2

qf(p = 0.90, df1 = 1, df2 = 5)
qf(p = 0.95, df1 = 1, df2 = 5)
qf(p = 0.99, df1 = 1, df2 = 5)

rpois(n = 100, l = 5)

x <- seq(-4, 4, l = 100)
y1 <- dt(x, df = 1)
y2 <- dt(x, df = 5)
y3 <- dt(x, df = 10)
y4 <- dt(x, df = 50)
y5 <- dt(x, df = 500)
plot(x, y1, type = "l")
lines(x, y2, type = "l", col = "blue")
lines(x, y3, type = "l", col = "green")
lines(x, y4, type = "l", col = "purple")
lines(x, y5, type = "l", col = "red")


# Ex3

data <- read.csv("Auto.data", header = T, sep = "")

dim(Auto) # To see the dimensions of the data set
nrow(Auto) # To see the number of rows
ncol(Auto) # To see the number of columns

Auto[1:4,] # The first 4 rows of the data set
# Once the data are loaded correctly, we can use names()
# to check the variable names.
names(Auto)
# Load the dataset, when we load an .RData using load()
# function we do not attribute it to a name like we did
# when we used read.table() or when we use read.csv()


# Ex4

load("EU.RData")
# lm (for linear model) has the syntax:
# lm(formula = response ~ predictor, data = data)
# The response is the y in the model. The predictor is x.
# For example (after loading the EU dataset)
mod <- lm(formula = Seats2011 ~ Population2010, data = EU)
# We have saved the linear model into mod, which now contains all the output of lm
# You can see it by typing
mod
# mod is indeed a list of objects whose names are
names(mod)
# We can access these elements by $
# For example
mod$coefficients
# Summary of the model
sumMod <- summary(mod)
sumMod


# Ex5

#We are going to use a dataset called Boston which is part of the MASS package. It recordes the 
#median value of houses for 506 neighborhoods around Boston. Our task is to predict the 
#median house value (medv) using
#only one predictor (lstat: percent of households with low socioeconomic status).
#First, install the MASS package using the command: install.packages("MASS")
#load MASS package
library(MASS)
# Check the dimensions of the Boston dataset
dim(Boston)

#STEP 1: Split the dataset

# Split the data by using the first 400 observations as the training
# data and the remaining as the testing data
train = 1:400
test = -train
# Speficy that we are going to use only two variables (lstat and medv)
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]
# Check the dimensions of the new dataset
dim(training_data)

#STEP 2: Check for Linearity

#In order to perfom linear regression in R, we will use the function lm()to fit a simple linear regression with
#medv as the response (dependent variable) and lstat as the predictor or independent variable, and then
#save it in model.
#But before we run our model, let's visually check if the relationship between x and y is linear.
# Scatterplot of lstat vs. medv
plot(training_data$lstat, training_data$medv)
#According to the plot, we see that the relationship is not linear. Let's try a transformation of our explanatory
#variable lstat.
# Scatterplot of log(lstat) vs. medv
plot(log(training_data$lstat), training_data$medv)
#Look at the plot, it is more linear, so we can proceed and perform lm():

#STEP 3: Run the linear regression model

model = lm(medv ~ log(lstat), data = training_data) #log pour rendre lstat lineaire
model
summary(model)

names(model)
model$coefficients
#To obtain the confidence intervel for the linear model (model), 
#we can use the confint() function:
confint(model, level = 0.95)

#STEP 4: Plot the regression model

#Now, let's plot our regression line on top of our data.
# Scatterplot of lstat vs. medv
plot(log(training_data$lstat), training_data$medv)
# Add the regression line to the existing scatterplot
abline(model)

# Scatterplot of lstat vs. medv
plot(log(training_data$lstat), training_data$medv,
     xlab = "Log Transform of % of Houshold with Low Socioeconomic Income",
     ylab = "Median House Value",
     col = "red",
     pch = 20)
# Make the line color blue, and the line's width =3 (play with the width!)
abline(model, col = "blue", lwd =3)

#STEP 5: Assess the model

#Final thing we will do is to predict using our fitted model. 
#We can use the predict() function for this purpose:
# Predict what is the median value of the house with lstat= 5%
predict(model, data.frame(lstat = c(5)))
# Predict what is the median values of houses with lstat= 5%, 10%, and 15%
predict(model, data.frame(lstat = c(5,10,15), interval = "prediction"))

#Now let's assess our model, by computing the mean squared error (MSE). 
#To assess the model we created, then we will be using the test data!

# Save the testing median values for houses (testing y) in y
y = testing_data$medv
# Compute the predicted value for this y (y hat)
y_hat = predict(model, data.frame(lstat = testing_data$lstat))
# Now we have both y and y_hat for our testing data.
# let's find the mean square error
error = y-y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE





