library(NbClust)
library(dplyr)
library(class)
library(cluster)
library(ggplot2)

seed <- seeds_dataset_header

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
mega_seed = data.frame(gg1$all, Species=as.factor(rep(seed$Label, length=nrow(gg1$all))))


#--// SCatter plot of our DBSCAN results \\--
ggplot(mega_seed, aes_string(x = "x", y = "y")) + 
  facet_grid(xvar ~ yvar, scale = "free") +
  geom_point(aes(colour=Species), na.rm = TRUE, alpha=0.8) +
  
  #Label colours and names
  scale_color_manual(values=c("#90AFC5", "#336B87", "#2A3132"),
                     labels=c("Kama", "Rosa", "Canadian"))+
  theme(
    #Background colour & remove axis titles 
    panel.background = element_rect(fill="#F1F1F2"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())