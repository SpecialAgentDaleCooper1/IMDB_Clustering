### Libraries ###

library(cluster)
library(tidyverse)
library(corrplot)
library(gridExtra)
library(GGally)
library(knitr)
library(ggfortify)

### Import Dataset ###

library(readr)
imdb <- read_csv("Desktop/imdb_top_1000.csv")
View(imdb)

# Data Cleaning

imdb <- imdb[ , -c(1:2 ,4:5 , 6 , 8 , 10 : 14)]
na.omit(imdb)
View(imdb)
summary(imdb)


# Histogram for each Attribute
imdb %>%
  gather(Attributes, value, 2:5) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Imdb Attributes - Histograms") +
  theme_bw()

# Density Plot for IMDB_Rating
imdb %>%
  gather(Attributes, value, "IMDB_Rating") %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Imdb Attributes - Histograms") +
  theme_bw()

# IMDB - Boxplots   
imdb %>%
  gather(Attributes, value, 2:5) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_boxplot(colour="black", show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Imdb Attributes - Histograms") +
  theme_bw()

# Correlation matrix 
corrplot(cor(imdb[2:5]), type="upper", method="square", tl.cex=0.9)

# Normalization
imdb_scale <- scale(imdb[2:5])
view(imdb_scale)

# Execution of k-means with k=2
set.seed(1234)
imdb_k2 <- kmeans(imdb_scale, centers=2)

imdb_k2$cluster
imdb_k2$centers
imdb_k2$size

# Sum Square Between
imdb_k2$betweenss

# Sum Square Within
imdb_k2$withinss


# Which number of k cluster is better??

bss <- numeric()
wss <- numeric()

# Run the algorithm for different values of k 
set.seed(1234)

for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(imdb_scale, centers=i)$betweenss
  wss[i] <- kmeans(imdb_scale, centers=i)$tot.withinss
  
}

# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Subplot
grid.arrange(p3, p4, ncol=2)

# Grafically, we can see that the best choice of k 
# happens to be when k = 3

# Cluster Plot with k = 3
autoplot(clara(imdb_scale, 3))




