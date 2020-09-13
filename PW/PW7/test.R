## Exercice 1

1. Download the dataset: Ligue1 2017-2018  and import it into R. Put the argument row.names to 1.
```{r}
setwd("/Users/abdallahessa/Desktop/ML.R")
ligue1 <- read.csv("ligue1_17_18.csv", row.names=1, sep=";")
```
2. Print the first two rows of the dataset and the total number of features in this dataset.
```{r}
knitr::kable(ligue1[1:2,])
```

## Exercice 2

3. We will first consider a smaller dataset to easily understand the results of k-means. Create a new dataset in which you consider only Points and Yellow.cards from the original dataset. Name it pointsCards
```{r}
pointsCards= ligue1$Points
pointsCards = cbind(pointsCards , ligue1$yellow.cards)
```
4. Apply k-means on pointsCards. Chose k=2 clusters and put the number of iterations to 20. Store your results into km. (Remark: kmeans() uses a random initialization of the clusters, so the results may vary from one call to another. Use set.seed() to have reproducible outputs).
```{r}
set.seed(1234)
km <- kmeans(pointsCards,centers=2,iter.max = 20)
print(km)

```
there is 2 clusters the first one is grouping 4 elements and the second one 16.

in the cluster means we can observe the coordinates of the center of each cluster

in clustering vector we can observe for each individu a value (1 or 2) which indicate if this indivedual belong respectevelie to the first or second cluster.

6. What are the coordinates of the centers of the clusters (called also prototypes or centroids) ?
  ```{r}
km$centers
```
the coordinates of the clusters are respectevely (82.00,71.2500) and (44.75,71.5625)

7. Plot the data (Yellow.cards vs Points). Color the points corresponding to their cluster.
```{r}
plot(pointsCards,col=km$cluster,pch=19,cex=2,main="k-means clustering solution using 2 clusters",xlab ="Points",ylab="yellow.cards")
```

8. Add to the previous plot the clusters centroids and add the names of the observations.

```{r}
#points(km$centers,col=1:2,pch=3,cex=3,lwd=3) 
#text(x= pointsCards, labels = rownames(pointsCards), col = km$cluster,pos=3,cex=0,75 = )
```


add the names of the observations.


9. Re-run k-means on pointsCards using 3 and 4 clusters and store the results into km3 and km4 respectively. Visualize the results like in question 7 and 8.
```{r}
km3 <- kmeans(pointsCards,centers=3,iter.max = 20)
km4 <- kmeans(pointsCards,centers=4,iter.max = 20)
plot(pointsCards,col=km3$cluster,pch=19,cex=2,main="k-means clustering solution using 3 clusters",xlab ="Points",ylab="yellow.cards")
#points(km3$centers,col=1:3,pch=3,cex=3,lwd=3) 
plot(pointsCards,col=km4$cluster,pch=19,cex=2,main="k-means clustering solution using 4 clusters",xlab ="Points",ylab="yellow.cards")
#points(km4$centers,col=1:4,pch=3,cex=3,lwd=3) 
```

10. Visualize the "within groups sum of squares" of the k-means clustering results.


```{r}
mydata <- pointsCards
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares", main="within groups sum of squares")
```

11. Modify the code of the previous question in order to visualize the "between_SS / total_SS". Interpret the results.

```{r}
wss <- 0
for (i in 1:15) wss[i] <- kmeans(mydata,centers=i)$betweenss/kmeans(mydata,centers=i)$totss
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="between_SS / total_SS", main="within groups sum of ")
```

## Exercice 3

12. Scale the dataset and transform it to a data frame again. Store the scaled dataset into ligue1_scaled.
```{r}
ligue1_scaled= scale(ligue1)
```

13. Apply kmeans() on ligue1 and on ligue1_scaled using 3 clusters and 20 iterations. Store the results into km.ligue1 and km.ligue1.scaled respectively (do not forget to set a seed)
```{r}
set.seed(123)
km.ligue1 <- kmeans(ligue1,centers=3,iter.max = 20)
km.ligue1.scaled <- kmeans(ligue1_scaled,centers=3,iter.max = 20)
```

How muny observations there are in each cluster of km.ligue1 and km.ligue1.scaled ? (you can use table()). Do you obtain the same results when you perform kmeans() on the scaled and unscaled data?
  
  ```{r}
knitr::kable(km.ligue1$size)
knitr::kable(km.ligue1.scaled$size)
cm = table(km.ligue1$cluster, km.ligue1.scaled$cluster)
knitr::kable(cm)
```

We obtained the same results when we performed kmeans() on the scaled and unscaled data, the clusters may just be presented in a different orders.

## Exercice 4

15. Apply PCA on ligue1 dataset and store you results in pcaligue1. Do we need to apply PCA on the scaled dataset? Justify your answer.

```{r}
pcaligue1=princomp(ligue1, cor=T) 
plot(pcaligue1) 
```

We do not need to apply PCA on the scaled dataset because using the correlation matrix is equivalent to standardizing each of the variables

16. Plot the observations and the variables on the first two principal components (biplot). Interpret the results.

```{r}
library("FactoMineR")
library("factoextra")
res.pca <- PCA(ligue1, graph = FALSE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 80))
```

```{r}
fviz_pca_var(res.pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, title= " variables on thefirst two principal components" )
```

```{r}
fviz_pca_ind (res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, title= "observations on the  first two principal components" )
```

```{r}
fviz_pca_biplot(res.pca, col.ind = "cos2",  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, title = "observations and variables on the  first two principal components" )
```



17. Visualize the teams on the first two principal components and color them with respect to their cluster.

```{r}
fviz_cluster(km.ligue1, data = ligue1,palette = c("red", "blue", "green"),  ggtheme = theme_minimal(), main = "Clustering Plot" )
```

18. Recall that the figure of question 17 is a visualization with PC1 and PC2 of the clustering done with all the variables, not on PC1 and PC2. Now apply the kmeans() clustering taking only the first two PCs instead the variables of original dataset. Visualize the results and compare with the question 17.

```{r}
ligue1.pca = res.pca$ind$coord
km.ligue1.pca <- kmeans(ligue1.pca[,1:2],centers=3,iter.max = 20)
plot(ligue1.pca[,1:2],col=km.ligue1.pca$cluster,pch=19,cex=2,main="k-means clustering solution using 2 clusters",xlab ="Points",ylab="yellow.cards")
```

## Exercice 5

19. Plot the observations.
```{r}
observations = c(1,1,0,5,6,4)
observations = cbind(observations,c(4,3,4,1,2,0))
plot(observations)
```

20. Randomly assign a cluster label to each observation. You can use the sample() command in R to do this. Report the cluster labels for each observation.

```{r}

```


```{r}

```

```{r}

```


```{r}

```

```{r}

```


```{r}

```

```{r}

```



```{r}
#x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
#y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
#plot(x,y,col="blue",pch=19,cex=2)
#text(x+0.05,y+0.05,labels=as.character(1:12))
#names(kmeansObj)
```

```{r}

```

```{r}

```







