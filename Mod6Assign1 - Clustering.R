library(tidyverse)
library(tidymodels)
library(cluster) #algorithms for clustering
library(factoextra) #visualization
library(dendextend) #viewing clustering dendograms
library(readr)
library(ggplot2)


TruckData = read.csv("trucks-1.csv")
head(TruckData,10)

ggplot(TruckData,aes(x=Distance,y=Speeding)) + geom_point()

TruckData = TruckData %>% select(-Driver_ID)
head(TruckData,5)
Trucks_Cleaned = scale(TruckData)
summary(Trucks_Cleaned)  

set.seed(412)
clusts = 
  tibble(k=1:8) %>% 
  mutate(
    kclust = map(k, ~kmeans(Trucks_Claned, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, Trucks_Claned)
  )
clusts

clusters = 
  clusts %>%
  unnest(cols = c(tidied))

assignments = 
  clusts %>% 
  unnest(cols = c(augmented))

clusterings = 
  clusts %>%
  unnest(cols = c(glanced))

p1 = 
  ggplot(assignments, aes(x = Distance, y = Speeding)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1


ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + geom_point() + theme_bw()


cust_clust = kmeans(Trucks_Cleaned, centers = 2) 
cust_clust #view results

customers = augment(cust_clust, Trucks_Cleaned)
head(customers)




