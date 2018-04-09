library(NbClust)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(reshape2)
library(plyr)

seed <-select(seeds_dataset_header, -Label)

#--// Segmentation of Datasets \\--
kama <-seed[1:70,]
rosa <- seed[71:140,]
canadian <- seed[141:210,]

#--// Combine data.frames \\--
df <- rbind(kama, rosa, canadian)

# Create variable Group. BASICALLY replaced label 1, 2 and 3 for their names.
df$Label <- rep(c("Kama", "Rosa", "Canadian"), c(dim(kama)[1], dim(rosa)[1], dim(canadian)[1]))

# Transform to long format.
attributes <- melt(df, "Label")

# --// Boxplots of each attribute \\--
#IF IT DOESN'T SHOW, RUN THIS INDIVIDUALLY.
ggplot(attributes, aes(x=Label, y=value, fill=Label)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  facet_wrap(~variable, scale="free")

#--// Correlation Plots of each Species \\--
corrplot(cor(kama), tl.pos = "d", method = "color", tl.col = "black", title = "Kama")
corrplot(cor(rosa), tl.pos = "d", method = "color", tl.col = "black", title = "Rosa")
corrplot(cor(canadian), tl.pos = "d", method = "color", tl.col = "black", title = "Canadian")

#--// Correlation between the correlation matrices.\\ --
cor(c(cor(kama)), c(cor(rosa)))

#--// Preparation for Correlation Plots BETWEEN species \\--
karo <- cor(kama, rosa)
kanadian <- cor(kama, canadian)
rosadian <- cor(rosa, canadian)

colnames(karo) <- c("A(K)", "P(K)", "C(K)","L(K)","W(K)","AC(K)","LG(K)")
rownames(karo) <- c("A(R)", "P(R)", "C(R)","L(R)","W(R)","AC(R)","LG(R)")

colnames(kanadian) <- c("A(K)", "P(K)", "C(K)","L(K)","W(K)","AC(K)","LG(K)")
rownames(kanadian) <- c("A(C)", "P(C)", "C(C)","L(C)","W(C)","AC(C)","LG(C)")

colnames(rosadian) <- c("A(R)", "P(R)", "C(R)","L(R)","W(R)","AC(R)","LG(R)")
rownames(rosadian) <- c("A(C)", "P(C)", "C(C)","L(C)","W(C)","AC(C)","LG(C)")

#--// Visualization of the Correlation Plots BETWEEN species \\--
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))

corrplot(karo, type = "upper",  tl.col = "black", method = "square", col = col4(20))
corrplot(kanadian, type = "upper",  tl.col = "black", method ="square", col = col4(20))
corrplot(rosadian, type = "upper", tl.col = "black", method ="square", col = col4(20))

#--// Means & Standard Deviation \\ --
meandata <- aggregate(seed[, 1:7], list(df$Label), mean)
sddata <- aggregate(seed[, 1:7], list(df$Label), sd)

# Data preparation
colnames(meandata)[1] <- "Label"
melted <- melt(df, id.vars="Label")
means <- ddply(melted, c("Label", "variable"), summarise, mean=mean(value), sem = sd(value))
means <- transform(means, lower=mean-sem, upper=mean+sem)

#--// Visualization of the Mean & Standard Deviation as BarPlots \\--
col <- colorRampPalette(c("#072859", "#09387d", "#08519c"))

ggplot(means, aes(x=variable, y=mean, fill=Label)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_linerange( aes(ymax=upper, ymin=lower), position=position_dodge(0.9),
                  colour="orange", alpha=0.9, size=1.3)+
  scale_fill_manual(values = c("#072859", "#08519c", "#6baed6"))

