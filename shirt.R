library("recommenderlab")
# Loading to pre-computed affinity data
download.file("https://ibm.box.com/shared/static/vw5gm9h25gnd7qdcmxm9exawphh0kgxy.csv", "/resources/data/HKkids.csv")
download.file("https://ibm.box.com/shared/static/vw5gm9h25gnd7qdcmxm9exawphh0kgxy.csv", "/resources/data/HKkids.csv")
setwd("C:/Users/Maroua/Desktop/Shirt")
HKkids <- read.csv(file = "kids", stringsAsFactors = FALSE, sep = ";")
getwd()
HKkids <- read.csv("kids.csv", sep =';')
head(HKkids)
# Changing column names to shorter names
colnames(HKkids) <- c("Index", "Height", "Weight")
head(HKkids)
# Let's check general information  about the data!
# Removing the unnecessary index column
HKkids$Index <- NULL
# install the ggplot2 package
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
library(ggplot2)

ggplot(HKkids, aes(x = HKkids$Height, y = HKkids$Weight)) +
  geom_point(shape=1, size = 2, color = "black", alpha = 1/3) +
  geom_point(size = 0.1, color = "red4", alpha = 1/3) +
  labs (x = "Height (Inches)", y = "Weight (Pounds)")

original_height_mean = mean(HKkids$Height)
original_height_sd = sd(HKkids$Height)
original_weight_mean = mean(HKkids$Weight)
original_weight_sd = sd(HKkids$Weight)
HKkids$Height <- (HKkids$Height - original_height_mean) / original_height_sd
HKkids$Weight <- (HKkids$Weight - original_weight_mean) / original_weight_sd
# "HKkids" is our data
# "centers" is the number of clusters
# "iter.max" is the number of iterations
# Try experimenting with different numbers!
clusters_numbers = kmeans (HKkids, centers = 3, iter.max = 10)
# We are using a color_offset = 4 just for making things easier to see.
# Try experimenting with different numbers!
color_offset = 4
# We have to convert numbers to categorical data in order to color the chart
HKkids$Cluster <- as.factor(clusters_numbers$cluster + color_offset)
head(HKkids)
ggplot() +
  geom_point(data = HKkids, aes(x = HKkids$Height, y = HKkids$Weight), shape=1, size = 2, color = "black", alpha = 1/2) +
  geom_point(data = HKkids, aes(x = HKkids$Height, y = HKkids$Weight), shape=1, size = 0.1, colour = HKkids$Cluster, alpha = 1/2) +
  labs (x = "Height (Inches)", y = "Weight (Pounds)")
# Extracting the cluster centroids
Cluster_centroids <- as.data.frame(clusters_numbers$centers)
# Plotting the centroids found
ggplot() +
  geom_point(data = HKkids, aes(x = HKkids$Height, y = HKkids$Weight), shape=1, size = 2, color = "black", alpha = 1/10) +
  geom_point(data = HKkids, aes(x = HKkids$Height, y = HKkids$Weight), shape=1, size = 0.1, colour = HKkids$Cluster, alpha = 1/4) +
  geom_point(data = Cluster_centroids, aes(x = Cluster_centroids$Height, y = Cluster_centroids$Weight), shape = 1, size = 3, color = "black") +
  geom_point(data = Cluster_centroids, aes(x = Cluster_centroids$Height, y = Cluster_centroids$Weight), shape = 1, size = 5, color = "black") +
  geom_point(data = Cluster_centroids, aes(x = Cluster_centroids$Height, y = Cluster_centroids$Weight), shape = 3, size = 10, color = "black") +
  labs (x = "Height (Inches)", y = "Weight (Pounds)")
# Undoing the normalization
tee_sizes <- Cluster_centroids
tee_sizes$Height <- (tee_sizes$Height * original_height_sd) + original_height_mean
tee_sizes$Weight <- (tee_sizes$Weight * original_weight_sd) + original_weight_mean
tee_sizes
small <- which(tee_sizes$Height==min(tee_sizes$Height))
tee_sizes[small,]
medium <- which(tee_sizes$Height==median(tee_sizes$Height))
tee_sizes[medium,]
large <- which(tee_sizes$Height==max(tee_sizes$Height))
tee_sizes[large,]

# Creating subsets for each one of the segments of the population
small_HKkids <- HKkids[HKkids$Cluster == small + color_offset,]
medium_HKkids <- HKkids[HKkids$Cluster == medium + color_offset,]
large_HKkids <- HKkids[HKkids$Cluster == large + color_offset,]
# Storing all the standard deviations of each cluster to our tee_size matrix
tee_sizes$sd_Height[small] = sd(small_HKkids$Height)
tee_sizes$sd_Height[medium] = sd(medium_HKkids$Height)
tee_sizes$sd_Height[large] = sd(large_HKkids$Height)
tee_sizes$sd_Weight[small] = sd(small_HKkids$Weight)
tee_sizes$sd_Weight[medium] = sd(medium_HKkids$Weight)
tee_sizes$sd_Weight[large] = sd(large_HKkids$Weight)
tee_sizes


