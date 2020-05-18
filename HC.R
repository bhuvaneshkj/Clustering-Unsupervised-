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
#view(df)

df <- X %>% sample_frac(0.02)
dd <- dist(df, method = "euclidean")

fit <- hclust(dd, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=2) # cut tree into 2 clusters
# draw dendogram with red borders around the 2 clusters
rect.hclust(fit, k=2, border="red")

agn1 <- agnes(df, metric = "euclidean", stand = FALSE)
agn1
#plot(agn1)
#print(agn1$dc)

diana1 <- diana(df, metric = "euclidean", stand = FALSE)
print(diana1$dc)


cluster.stats(dd,agn1$cluster)
