library(dbscan)
library(dplyr)
library(ggplot2)


seed <- select(seeds_dataset_header, -Label)

#--//Compute DBSCAN\\--
w =dbscan(seed, eps = 1.23, minPts = 23)

#--//kNN Distance Plot to understand where the vaue of eps should be\\--
kNNdistplot(seed, k = 23) + abline(h=1.23)

#--//Considers the noise as a fourth cluster\\--
w$cluster[w$cluster == 0] <- 4

#--//Function to later on visualize a plot Matrix\\--
#--//ADAPTED FROM https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/ \\--
makePairs <- function(data) 
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  list(all=all)
}

#--// Prepare data for Scatter Plot Matrix \\--
gg1 = makePairs(seed[,-8])
mega_seed = data.frame(gg1$all, Species=as.factor(rep(w$cluster, length=nrow(gg1$all))))

#--// SCatter plot of our DBSCAN results \\--
ggplot(mega_seed, aes_string(x = "x", y = "y")) + 
  facet_grid(xvar ~ yvar, scale = "free") +
  geom_point(aes(colour=Species), na.rm = TRUE, alpha=0.8) +
  
  #Label colours and names
  scale_color_manual(values=c("#04202C", "#304040", "#5B7065", "red"),
                     labels=c("Cluster 1", "Cluster 2", "Cluster 3", "no label"))+
  theme(
    #Background colour & remove axis titles 
    panel.background = element_rect(fill="#F1F1F2"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())


#--//Silhouette Vizualization\\--
sil_KM <- silhouette(w$cluster, dist(seed))
fviz_silhouette(sil_KM,  palette = "Set2")

#--// Matrix of Contour Plots \\--
#--//Label is not included/does not affect the contour plot.
ggplot(mega_seed, aes(x=x, y=y) ) +
  geom_density2d() +
  stat_density2d(aes(fill = ..level.., alpha = ..level..),size = 0.01, bins = 8, geom = 'polygon', colour = "white") +
  #scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.25, 0.5), guide = FALSE)+
  facet_grid(xvar ~ yvar, scale = "free")
