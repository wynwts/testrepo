setwd("/Users/claire/Documents/Columbia University/5205 AA Frameworks & Methods II/Assignments/Assignment 1")
data <- read.csv('fastfood_survey.csv')
library(tidyverse)

######################3###Assignment 1-1
library(stats)
dist <- stats::dist
#Q1
str(data)
#Q2 contain first eleven variables in the dataset, Call this data_cluster.
data_cluster = data[, 1:11]
str(data_cluster)
#Q3  number if NA in column of cleanliness
summary(data$cleanliness)
sum(is.na(data$cleanliness))
#Q4 left if rows corresponding to missing values on any of the eleven variables were removed
data_na <- na.omit(data_cluster)
str(data_na)

#Q5 imputed value of observation 10 for the variable cleanliness
#install.packages('mice') 
library(mice)
set.seed(1706)
data_cluster = complete(mice(data_cluster, use.matcher=T))
head(data_cluster$cleanliness, 10)
#6 standardize the variables- value of observation 10 for the variable cleanliness
data_cluster = scale(data_cluster)
data_cluster[1:10,]


########################### Assignment 1-2
#Q1 Euclidean distance between all observations in data_cluster-
#How many elements are in the distance matrix==dist' num [1:193131]
d <-   stats::dist(x = data_cluster,method = 'euclidean')
hclust(d, method = "complete")
str(d) 
#Q2 What is the Cophenetic correlation coefficient
clusters = hclust(d = d, method = 'ward.D2')
cor(cophenetic(clusters),d)

#Q3  which is the best cluster solution--Between cluster distance to be large
plot(cut(as.dendrogram(clusters),h=5)$upper)
rect.hclust(tree=clusters,k = 2,border='tomato')

#Q4: how many observations would be in the smaller of the two clusters?
h2_segments = cutree(tree = clusters, k=2)
table(h2_segments)
#5 three-cluster solution, how many observations would be in the smallest of the three clusters
h3_segments = cutree(tree = clusters, k=3)
table(h3_segments)


##############Clustering - K Means
#6:How many observations are in the smaller cluster?-K mean 2 clusters
set.seed(1706)
km_2= kmeans(x = data_cluster,centers = 2,iter.max=100)
table(km_2$cluster)
#Q7:How many observations are in the smallest cluster?-K mean 3 clusters
set.seed(1706)
km_3= kmeans(x = data_cluster,centers = 3,iter.max=100)
table(km_3$cluster)

km_3$tot.withinss


#Q8:Compute the total within cluster sum of squares for cluster solutions from 2 to 10. Use a seed of 1706. Do not set nstart. What is the total within cluster sum of squares for a three-cluster solution?
within_ss = sapply(2:10,FUN = function(x){set.seed(1706)
  +     kmeans(x = data_cluster,centers = x,iter.max = 100)$tot.withinss})
within_ss[2]

OR

km_3$tot.withinss

#Q9:For the three-cluster solution, what is the ratio of between sum of squares and total sum of squares?
ratio_ss = sapply(2:10,FUN = function(x) {
  set.seed(1706)
  km = kmeans(x = data_cluster,centers = x,iter.max = 100)
  km$betweenss/km$totss} )
ratio_ss[2]

OR

km_3$betweenss/(km_3$betweenss + km_3$tot.withinss)

#Q10:Construct a line graph of clusters (on x-axis) against total within cluster sum of squares (on y-axis). Based on this chart, which of the following are good cluster solutions?--- Slope
ggplot(data=data.frame(cluster = 2:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Q11:What is the average silhouette width for a 2 cluster solution?
library(cluster)
pam(data_cluster,k = 2)$silinfo$avg.width

#Q12:What is the average silhouette width for a 3 cluster solution?
pam(data_cluster,k = 3)$silinfo$avg.width


#Q13:Based on this criterion, which is the best cluster solution? - ALways pick the highest silhouette width
silhoette_width = sapply(2:10,FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)

ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))



#Q14:How many clusters has the model decided to group the data into?
#install.packages("mclust")
library(mclust) 
clusters_mclust = Mclust(data_cluster)
 
#Section2-Q15: model-based clustering to force a two-cluster solution
clusters_mclust_2 = Mclust(data_cluster,G=2)
summary(clusters_mclust_2)

#Q16-For how many observations do the cluster assignments differ? KvsH
table(h2_segments)   #hierarchical
table(km_2$cluster)   #kmeans

min(sum(h2_segments==km_2$cluster),
    sum(h2_segments!=km_2$cluster))


# mClustering table:
#  1   2 
# 451 171 

# k-Clustering table:
# k_segments
# 1   2 
# 579  43 



#Q17-For how many observations do the cluster assignments differ? K vs M
table(m_segments) #Model-based
table(km_2$cluster) #«K-means

min(sum(m_segments==km_2$cluster),sum(m_segments!=km_2$cluster))




########################## Assignment  1-3
#Q1-4:combine the cluster memberships from three-cluster k-means with the original dataset

library(dplyr); library(ggplot2); library(tidyr)

data2 <- cbind(km_3$cluster,data)
data2%>%
  select(speed_of_service:taste_burgers,km_3$cluster)%>%
  group_by(km_3$cluster)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()

data2 <- cbind(km_3$cluster,data)
data2%>%
  select(speed_of_service:taste_burgers,km_3$cluster)%>%
  group_by(km_3$cluster)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key= var, value = value, speed_of_service:taste_burgers) %>%
  ggplot(aes(x=var, y=value, fill=factor(km_3$cluster)))+ geom_col(position='dodge')+ coord_flip() 




#Section3-Q5-7: examine distributions of factor demographic variables
lapply(13:22,function(x) round(prop.table(table(data$`km_3$cluster`,data[,x]),1),2)*100)