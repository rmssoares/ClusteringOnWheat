library(Rcpp)
library(class)
library(ggplot2)
library(factoextra)
library(fpc)
library(NbClust)
library(EMCluster)
library(mclust)
library(cluster)
library(dendextend)
library(descr)
library(ClusterR)


#-------- Data import (Run data_import.R First) -------------
df <- seedunlabeled
df <- scale(df)
#------------------------------------------------------------


#-----------------Clustering Techniques----------------------
#--//K-Means Clustering\\--
km.res <- eclust(df, "kmeans", k = 3, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",palette = "jco", ggtheme = theme_minimal())

#--//Hierarchical clustering\\--
hc.res <- eclust(df, "hclust", k = 3, hc_metric = "euclidean", hc_method = "ward.D2", graph = FALSE);
hc1.res <-agnes(df, method = "average")
fviz_dend(hc1.res, show_labels = FALSE,palette = "jco", as.ggplot = TRUE, main = "AGNES Dendogram")
hc2.res <- diana(df)
fviz_dend(hc2.res, show_labels = FALSE,palette = "jco", as.ggplot = TRUE, main = "DIANA Dendogram")

#--//EM Clustering\\--
#Adapted from https://mran.microsoft.com/snapshot/2016-08-19/web/packages/mclust/vignettes/mclust.html
#WARNING: R 3.4.4 AND UPDATED LIBRARIES ARE NEEDED.
mod1 <- Mclust(df, x = BIC)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")
table(seeds_dataset_header$Label, mod1$classification)
plot(mod1, what = "uncertainty")
ICL = mclustICL(df)
summary(ICL)
plot(ICL)
mod5 <- densityMclust(df)
summary(mod5)
plot(mod5, what = "density")


#--//KNN\\--
#Adapted by https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
seed_norm <- as.data.frame((seedlabeled[1:7]))
summary(seed_norm)
str(seed_norm)
ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.67, 0.33))
seed.training <- seed_norm[ind==1, 1:7]
seed.test <- seed_norm[ind==2, 1:7]
seed.trainLabels <- seedlabeled[ind==1, 8]
seed.testLabels <- seedlabeled[ind==2, 8]
seed_pred.res <- knn(train = seed.training, test = seed.test, cl = t(seed.trainLabels), k=3)
plot(seed_pred.res)
#------------------------------------------------------------


#-----------------Validation Techniques----------------------
#Adapted by http://www.sthda.com/english/articles/29-cluster-validation-essentials/97-cluster-validation-statistics-must-know-methods/
# Silhouette information
silinfo <- km.res$silinfo;
names(silinfo);
head(silinfo$widths[, 1:3], 10);
silinfo$clus.avg.widths;
silinfo$avg.width;
km.res$size;
sil <- km.res$silinfo$widths[, 1:3];
neg_sil_index <- which(sil[, 'sil_width'] < 0);
sil[neg_sil_index, , drop = FALSE];

#--//Silhouette Vizualization\\--
#K-Means
sil_KM <- silhouette(km.res$cluster, dist(df))
fviz_silhouette(sil_KM)
#Hierarchical
sil_HC <- silhouette(hc.res$cluster, dist(df))
fviz_silhouette(sil_HC)


#--//TangleGram\\--
#Hierarchical
dend1 <- as.dendrogram(hc1.res);
dend2 <- as.dendrogram(hc2.res);
tanglegram(dend1, dend2, main_left = "Agnes", main_right = "Diana", lwd = 0.01,highlight_distinct_edges = FALSE,
           type = "r", dLeaf = 0 , margin_inner = 0.1)

#--//Crosstable\\--
#KNN
compare <- cbind(seed_pred,seed.testLabels)
res_cross = CrossTable(x = seed_pred, y = t(seed.testLabels) , prop.chisq=FALSE)
#-------------------------------------------------------------

#--//External Validation\\--(Adapted by https://cran.r-project.org/web/packages/hdnom/vignettes/hdnom.html)
#-------------------------------------------------------------
res_km = external_validation(res.km$cluster, seeds_dataset_header$Label, method = "rand_index", summary_stats = T)
res_hc = external_validation(hc.res$cluster, seeds_dataset_header$Label, method = "rand_index", summary_stats = T)
res_EM = external_validation(mod1$classification, seeds_dataset_header$Label, method = "rand_index", summary_stats = T)
