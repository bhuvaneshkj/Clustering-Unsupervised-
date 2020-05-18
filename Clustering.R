library(dplyr)
library(tidyverse)
#install.packages("ggplot2")
#library("factoextra")
library("useful")
library("fpc")
#install.packages(("cluster", "factoextra"))
library("cluster")
library("factoextra")
library("ggplot2")

data <- read.csv("C:/Users/chat2/Downloads/dataset-har-PUC-Rio-ugulino/dataset-har-PUC-Rio-ugulino.csv",sep = ";")
X <- data %>% select(7:18)
df <- X %>% sample_frac(0.02)
#view(df)

dd <- dist(df, method = "euclidean")
factoextra::fviz_nbclust(
  x = df,
  FUNcluster = kmeans,
  method = "wss"
)

#k =2



kmeans_2 <- kmeans(
  x = df,
  centers = 2
)

  print(kmeans_2$betweenss)
useful::plot.kmeans(
  x = kmeans_2,
  data = df  
)

fpc::cluster.stats(
  d = dist(df),
  clustering = kmeans_2$cluster,
  G2 = FALSE,
  G3 = FALSE
)


clara_2 <- clara(df, 2, metric = "euclidean", stand = FALSE, 
      samples = nrow(df), pamLike = FALSE)

#fviz_cluster(clara_2, data=df)


fanny_2 <- fanny(df, 2, metric = "euclidean", stand = FALSE)

print(fanny_2)

#fviz_cluster(fanny_2, data =df)

  
cluster.stats(dd,pam_2$cluster)
  
pam_2 <- pam(df, 2, metric = "euclidean", stand = FALSE)

fviz_cluster(pam_2, data =df)