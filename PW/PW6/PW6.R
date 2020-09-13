# PW6

# Leo Casanova

# 23-11-2018


# THE IRIS DATASET


# Loading Data

# 1. Download the iris dataset and import it into R.

dataset <- read.csv("iris.data")
head(dataset)


# Exploratory analysis

# 2. Compare the means and the quartiles of the 3 different flower classes for the 4 different features (Plot 4 boxplots into the same figure).\

par(mfrow=c(2,2))
boxplot(dataset$sepal_length ~ dataset$class, ylab = "Sepal length")
boxplot(dataset$sepal_width ~ dataset$class, ylab = "Sepal width")
boxplot(dataset$petal_length ~ dataset$class, ylab = "Petal length")
boxplot(dataset$petal_width ~ dataset$class, ylab = "Petal width")


# 3. To explore how the 3 different flower classes are distributed along the 4 different features, visualize them via histograms using the following code.

library(ggplot2)

# histogram of sepal_length
ggplot(dataset, aes(x=sepal_length, fill=class)) + geom_histogram(binwidth=.2, alpha=.5)
# histogram of sepal_width
ggplot(dataset, aes(x=sepal_width, fill=class)) + geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_length
ggplot(dataset, aes(x=petal_length, fill=class)) + geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_width
ggplot(dataset, aes(x=petal_width, fill=class)) + geom_histogram(binwidth=.2, alpha=.5)


# PCA using princomp()

# 4. Apply a PCA on the Iris dataset using the princomp().

pcairis=princomp(dataset[,-5], cor=T) 
str(pcairis)
summary(pcairis)
#Proportion of Variance : Comp.1 = 0.73 and Comp.2 = 0.23
#With the cumulative proportion with both Comp.1 % Comp.2 we have 95,8% so it is very precise.
plot(pcairis) 
biplot(pcairis) 
# sepal_width and the other variables are not correlated much.
# petal_with and petal_length are strongly correlated.

# Deeper PCA using factoextra package

# 5. Using factoextra packag plot the following:

library("FactoMineR")
library("factoextra")

res.pca <- PCA(dataset[,-5], graph = FALSE)
print(res.pca)

# Screeplot
eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
#We can see the same result on the graphic, the first 2 dimensions represent > 95% of the variance.

# graph of individuals
ind <- get_pca_ind(res.pca)
ind

fviz_pca_ind(res.pca)

# graph of variables
var <- get_pca_var(res.pca)
var

fviz_pca_var(res.pca)

# biplot graph
fviz_pca_biplot(res.pca)

# Again, we can see that sepal_width and the other variables are not correlated much.
# petal_with and petal_length are strongly correlated.

head(var$contrib)
fviz_contrib(res.pca, choice = "var", axes = 1:2)
# The contribution of the variables to the first 2 dimensions are almost similar.


# Step-by-step PCA

# 6. First step, split the iris dataset into data X and class labels y.

X <- dataset[,-5]
y <- dataset[,5]


# Standardizing

#7. Scale the 4 features. Store the scaled matrix into a new one (for example, name it X_scaled).

X_scaled <- scale(X)
head(X_scaled)


# Covariance Matrix

# 8. The classic approach to PCA is to perform the eigendecomposition on the covariance matrix ??, which is a p × p matrix
# where each element represents the covariance between two features. Compute the Covariance Matrix of the scaled features (Print the results).

cov_X_scaled <- cov(X_scaled)
cov_X_scaled


# 9. Perform an eigendecomposition on the covariance matrix. Compute the Eigenvectors and the Eigenvalues (you can use the eigen() function).
#What do you obtain?

eigendecomposition_cov <- eigen(cov_X_scaled)
eigendecomposition_cov
#We obtain the eigenvalues of all the eigenvectors.


# Correlation Matrix

# 10. Perform an eigendecomposition of the standardized data based on the correlation matrix.

cor_X_scaled = cor(X_scaled)
eigendecomposition_cor <- eigen(cor_X_scaled)
eigendecomposition_cor
#We can see that the covariance matrix and the correlation matrix are exactly the same and so are the eigenvalues.


# 11. Perform an eigendecomposition of the raw data based on the correlation matrix. Compare the obtained results with the previous question.

cor_X <- cor(X)
eigendecomposition_rawdata_cor <- eigen(cor_X)
eigendecomposition_rawdata_cor
#The raw data based correlation matrix gives the same results as the scaled correlation matrix.


# Explained Variance

# 12. Calculate the individual explained variation and the cumulative explained variation of each principal component. Show the results.

ind_explained_var <- eigendecomposition_cov$values/sum(eigendecomposition_cov$values)
ind_explained_var
cum_explained_var <- cumsum(ind_explained_var)
cum_explained_var
#We obtain the same result as previously obtained with PCA functions.


# 13. Plot the individual explained variation. (scree plot)

plot(ind_explained_var, type = "b")


# Projection Matrix

# 14. Construct the projection matrix that will be used to transform the Iris data onto the new feature subspace.

pm = eigendecomposition_cov$vectors[,1:2]
pm


# Projection Onto the New Feature Space

# 15. Compute Y (Recall the Y is the matrix of scores, A is the matrix of loadings).

Y = X_scaled %*%pm
Y


# 16. Plot the observations on the new feature space. Name the axis PC1 and PC2.

plot(Y, xlab = "PC1", ylab = "PC2")


# 17. On the same plot, color the observations (the flowers) with respect to their flower classes.

plot(Y, xlab = "PC1", ylab = "PC2", col = y)
