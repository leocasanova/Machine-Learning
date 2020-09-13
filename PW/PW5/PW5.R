# PW5

# Leo Casanova

# 30-10-2018


#TITANIC DATASET STUDY

#First, let's do the pre-processing steps.
dataset = read.csv(file="Titanic.csv")

str(dataset)
summary(dataset)

#We can create a matrix of correlation to determine if there are relationships between numeric variables.
MC.subset = dataset[c(2, 3, 6, 7, 8, 10, 15)]
library(corrplot)
corrplot.mixed(cor(MC.subset))

#We now see that there are some correlation between the variables "Survived" and "Pclass" and also between "Survived" and "Fare".

#We can now create some plots to study the situation better and do the Anova test to see if the relationships are relevant.
boxplot(Pclass ~ Survived, data = dataset, col = "blue", main="Boxplot Pclass ~ Survived")

summary(aov(Pclass ~ Survived, data = dataset))
#The p-value is very low (< 0.05) so we can confirm there are some relationship between these 2 variables.
boxplot(Fare ~ Survived, data = dataset, col = "red", main = "Boxplot Fare ~ Survived")

summary(aov(Fare ~ Survived, data = dataset))
#The p-value is very low (< 0.05) so we can confirm there are some relationship between these 2 variables.

boxplot(Age ~ Survived, data = dataset, col = "green", main = "Boxplot Age ~ Survived")
summary(aov(Age ~ Survived, data = dataset))
#I would have thought that there would be some relationship between "Survived" and "Age" but we can now reject that hypotesis, as we can see, p-value > 0.05

#Now we have to see if there are some correlations between "Survived" and the other non-numeric variables.
table(dataset$Sex,dataset$Survived)
mosaicplot(~ Survived + Sex, data=dataset, main = "MosaicPlot of two categorical variables: Survived & Sex",color = 2:3, las = 1)
#We can clearly see that people with Sex = "female" are more likely to survive.

chisq.test(dataset$Survived, dataset$Sex)
#The Chi-square test confirms that there are some correlation between "Survived" and "Sex" with a low p-value (< 0.05)

table(dataset$Embarked,dataset$Survived)
mosaicplot(~ Survived + Embarked, data=dataset, main = "MosaicPlot of two categorical variables: Survived & Embarked",color = 2:4, las = 1)
chisq.test(dataset$Survived, dataset$Embarked)
#It seems that there are some correlation between "Survived" and "Embarked" as well. (p-value < 0.05)

table(dataset$FsizeD,dataset$Survived)
mosaicplot(~ Survived + FsizeD, data=dataset, main = "MosaicPlot of two categorical variables: Survived & FsizeD",color = 2:4, las = 1)
chisq.test(dataset$Survived, dataset$FsizeD)
#It seems that there are some correlation between "Survived" and "FsizeD" as well. (p-value < 0.05)

table(dataset$Title,dataset$Survived)
mosaicplot(~ Survived + Title, data=dataset, main = "MosaicPlot of two categorical variables: Survived & Title",color = 2:6, las = 1)
chisq.test(dataset$Survived, dataset$Title)
#It seems that there are some correlation between "Survived" and "Title" as well. (p-value < 0.05)

table(dataset$Child,dataset$Survived)
mosaicplot(~ Survived + Child, data=dataset, main = "MosaicPlot of two categorical variables: Survived & Child",color = 2:3, las = 1)
chisq.test(dataset$Survived, dataset$Child)
#It seems that there are some correlation between "Survived" and "Child" as well. (p-value < 0.05)

table(dataset$Mother,dataset$Survived)
mosaicplot(~ Survived + Mother, data=dataset, main = "MosaicPlot of two categorical variables: Survived & Mother",color = 2:3, las = 1)
chisq.test(dataset$Survived, dataset$Mother)
#It seems that there are some correlation between "Survived" and "Mother" as well. (p-value < 0.05)

#We will now split our dataset in order to run our different analysis.
#First lets create our new dataset with only the variables that are relevant for our study.
dataset = dataset[c(2, 3, 5, 10, 12, 13, 17, 19, 20)]
head(dataset)
str(dataset)
summary(dataset)

library(caTools)
set.seed(211575) 
split = sample.split(dataset$Survived, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#LOGISTIC REGRESSION
test1 = glm(Survived ~ Fare, family = binomial, data = training_set)
test1

test2 = glm(Survived ~ Fare + Pclass, family = binomial, data = training_set)
test2

#We can see that the AIC decreases as the number of variables we use increases, it means that the model is more and more accurate each time we add a variable.

classifier.logreg <- glm(Survived ~ . , family = binomial, data=training_set)
classifier.logreg
summary(classifier.logreg)

#Prediction
pred.glm = predict(classifier.logreg, newdata = test_set[,-1], type="response")

pred.glm_0_1 = ifelse(pred.glm >= 0.5, 1,0)

head(pred.glm)
head(pred.glm_0_1)

#Confusion matrix
cm = table(pred.glm_0_1, test_set[,1])
cm

mosaicplot(cm,col=sample(1:8,2))

accuracy = sum(diag(cm))/sum(cm)
accuracy

#ROC
require(ROCR)
score <- prediction(pred.glm,test_set[,1])
performance(score,"auc")
plot(performance(score,"tpr","fpr"),col="green")
abline(0,1,lty=8)
#We can now see the performance of our Logistic regression which we will be able to compare to the other methods later.


#LINEAR DISCRIMINANT ANALYSIS (LDA)

#We will now fit a LDA model 
library(MASS)
classifier.lda <- lda(Survived~., data = training_set)
classifier.lda

#Now we will predict the probability of surviving using the model classifier.lda on the test set.
predict.lda <- predict(classifier.lda, test_set[-1])
str(predict.lda)

cm.lda = table(predict.lda$class, test_set[,1])
cm.lda

mosaicplot(cm.lda,col=sample(1:8,2))

accuracy = sum(diag(cm.lda))/sum(cm.lda)
accuracy
#The accuracy is slightly better for the lda model compared to the logistic regression model.


#QUADRATIC DISCRIMINANT ANALYSIS (QDA)

#We will now fit a QDA model 
classifier.qda <- qda(Survived~. - Sex, data = training_set)
#I had to remove the variable "Sex" because it would not work otherwise with all the 9 variables : 
#"Error in qda.default(x, grouping, ...) : rank deficiency in group 0"
classifier.qda

#Now we will predict the probability of surviving using the model classifier.qda on the test set.
predict.qda <- predict(classifier.qda, test_set[-1])
str(predict.qda)

cm.qda = table(predict.qda$class, test_set[,1])
cm.qda 

accuracy = sum(diag(cm.qda))/sum(cm.qda)
accuracy
#We can see that the accuracy is exactly the same for the qda model and the lda model.

#COMPARISON

#In order to compare the methods we used, we are going to plot on the same figure the ROC curve for each classifier we fitted and compare the correspondant AUC.

score.glm <- prediction(pred.glm,test_set[,1])
score.lda <- prediction(predict.lda$posterior[,2],test_set[,1])
score.qda <- prediction(predict.qda$posterior[,2],test_set[,1])

performance(score.glm,"auc")
performance(score.lda,"auc")
performance(score.qda,"auc")

plot(performance(score.glm,"tpr","fpr"),col="green")
plot(performance(score.lda,"tpr","fpr"),col="red", add = TRUE)
plot(performance(score.qda,"tpr","fpr"),col="blue", add = TRUE)
abline(0,1,lty=8)

#There are no remarquable difference between each methods, they are all performing with more or less the same accuracy.
#It seems that they all have almost an equal "true positive rate".

#To conclude, we can say that the the passengers that are more likely to survive are the following : 
#Upper class ticket holders
#High passenger fare
#Females
#Port of Embarkation C
#Small FsizeD
#Miss & Mrs, and also to a lesser extent, Masters & rare title holders
#Children
#Mothers


