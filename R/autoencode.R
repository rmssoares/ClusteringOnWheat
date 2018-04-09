library(h2o)
library(dbscan)
library(dplyr)
library(ggplot2)
library(cluster)
library(eclust)
library(factoextra)
library(descr)
library(ClusterR)

seed <- seeds_dataset_header

#--//Initialize H2O Cluster (Refer to Report or further information or at h2o.ai documentation: http://docs.h2o.ai/h2o/latest-stable/h2o-docs/index.html)
h2o.init(nthreads=-1, max_mem_size="2G")

#--//Make sure Cluster is initialized EMPTY\\--
h2o.removeAll()
seedh2o <- as.h2o(seed)


#Every feature besides label
response <- "Label"
features <- setdiff(colnames(seedh2o), response)

#trains unsupervised since x=features does not include Label(Adapted from: https://shiring.github.io/machine_learning/2017/05/01/fraud)
model_nn <- h2o.deeplearning(x = features,
                             training_frame = seedh2o,
                             model_id = "model_nn",
                             hidden = c(4,2,4),
                             autoencoder = T,
                             activation="Tanh",
                             epochs = 100,
                             seed = 42)

model_nn


#--// Feature Space as extracted from the Intermediate Layer \\--
train_features <- h2o.deepfeatures(model_nn, seedh2o, layer = 2) %>%
  as.data.frame() 
train_features

#--//Clustering and Visualization\\--
colnames(train_features) <- c("FeatureX", "FeatureY")
km.res <- eclust(train_features, "kmeans", k = 3, nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",palette = "jco", ggtheme = theme_minimal())

#--//Silhouette Visualization\\--
sil_KM <- silhouette(km.res$cluster, dist(train_features))
fviz_silhouette(sil_KM,  palette = "Set2")

CrossTable(x = km.res$cluster, y = t(cbind(seed[,8])) , prop.chisq=FALSE)

#--//Scatterplot WITHOUT labels, of new Feature Space\\--
ggplot(train_features, aes(x = FeatureX, y = FeatureY, color=FeatureX))+
  geom_point(shape = 16, size = 3, show.legend = FALSE) +
  theme_minimal()+
  scale_color_gradient(low = "#0091ff", high = "#f0650e")

#--//Scatterplot WITH REAL labels added\\--
train_wlabels <- cbind(train_features, as.factor(cbind(seed[,8])))
colnames(train_wlabels) <- c("FeatureX", "FeatureY", "Species")

ggplot(train_wlabels, aes(x = FeatureX, y = FeatureY, shape = Species, colour = Species))+
geom_point(size=3, show.legend = FALSE) +
  scale_color_manual(values=c("#A0B084", "#A57C65", "#688B8A"),
                     labels=c("Kama", "Rosa", "Canadian"))+
scale_shape_manual(values=c(7, 8, 9),
                   labels=c("Kama", "Rosa", "Canadian"))

#--//External Validation\\--
res_km = external_validation(km.res$cluster, seeds_dataset_header$Label, method = "rand_index", summary_stats = T)
