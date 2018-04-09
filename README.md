# ClusteringOnWheat
*ClusteringOnWheat* is a project that represents several methods of Clustering on a dataset of *Wheat kernels*. As seen in the report, the **Clustering techniques** used were as follows:

* *K-Means.*
* *Hierarchical Clustering.*
* *EM.*
* *K-Nearest Neighbours.*
* *DBSCAN.*
* *K-Means of the feature space obtained after using Autoencoders for dimensionality reduction.*

The present report would extend in *EDA* and the implementation of each technique.

## Getting Started

Every implementation was developed in R. To start, clone the present repository into your local machine. If you're unaware of how to achieve this, please become familiar with the mechanisms of [GitHub](https://help.github.com/articles/set-up-git) repositories.

```
git clone git@github.com:thyriki/ClusteringOnWheat.git
```

### Prerequisites

Ensure that you have at least the **version 3.4.4** of [R](https://www.r-project.org/) installed and properly set up.

To easily program in *R*, [RStudio](https://www.rstudio.com/) was used.

The [h2o](http://docs.h2o.ai/h2o/latest-stable/h2o-docs/index.html) platform is used for the [autoencode.R](https://github.com/thyriki/ClusteringOnWheat/blob/master/R/autoencode.R) *R script*, and it initialises a *JVM*. As such, [Java JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html) must also be installed and properly set up. The script was done with *Java version 8 - 64 bits*.

Every script should be easy to run, after installing the necessary libraries (as found at the start of each script).

For any inquiries, feel free to open up an issue.

## Authors

* **Ricardo Soares** - [thyriki](https://github.com/thyriki)

* **Stelios Mouratidis** - [SteliosMouratidis](https://github.com/SteliosMouratidis)