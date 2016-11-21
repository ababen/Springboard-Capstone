# Removing links, retweets, hashtags, @people, punctuations, numbers, emojis, non-english characters and spaces

clean.string <- function(x)
{
  library(stringr)
  tweet = tolower(tweet)
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  tweet = gsub("#\\w+", " ", tweet)
  tweet = gsub("@\\w+", " ", tweet)
  tweet = gsub("[[:punct:]]", " ", tweet)
  tweet = gsub("[[:digit:]]", " ", tweet)
  tweet = gsub("the", " ", tweet)
  tweet = gsub("...", " ", tweet)
  tweet = gsub("&amp", " ", tweet)
  tweet <- str_replace_all(tweet,"[^[:graph:]]"," ")
  tweet <- str_replace_all(tweet,'https'," ")
  tweet1 <- grep('tweet',iconv(tweet,'latin1','ASCII',sub='tweet'))
  tweet  <- tweet[-tweet1]
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  rm(tweet1)
  return(x)
}

library(proxy) # Library required to use method = cosine in dist()

cosine <- function(x)
{
  cp <- crossprod(x)
  rtdg <- sqrt(diag(cp))
  cos <- cp / tcrossprod(rtdg)
  return(cos)
}

# --------Amit's Code--------------Determine Clusters-------------------------------------------
DetermineClusters <- function(x)
{
  wss <- (nrow(distMatrix)-1)*sum(apply(distMatrix,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(distMatrix,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  
  
  # 2. You can do partitioning around medoids to estimate the number of clusters using the pamk function in the fpc package.
  
  library(fpc)
  pamk.best <- pamk(distMatrix)
  cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
  plot(pam(distMatrix, pamk.best$nc))
  
  
  
  # 3.
  
  # we could also do:
  library(fpc)
  asw <- numeric(20)
  for (k in 3:20) asw[[k]] <- pam(distMatrix, k)$silinfo$avg.width
  k.best <- which.max(asw)
  cat("silhouette-optimal number of clusters:", k.best, "\n")
  
  # 4. 
  
  require(vegan)
  fit <- cascadeKM(scale(distMatrix, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
  plot(fit, sortg = TRUE, grpmts.plot = TRUE)
  calinski.best <- as.numeric(which.max(fit$results[2,]))
  cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
  
  # 5.
  
  # Determine the optimal model and number of clusters according to the Bayesian Information Criterion for 
  # expectation-maximization, initialized by hierarchical clustering for parameterized Gaussian mixture models
  library(mclust)
  # Run the function to see how many clusters
  # it finds to be optimal, set it to search for
  # at least 1 model and up 20.
  d_clust <- Mclust(as.matrix(distMatrix), G=1:20)
  m.best <- dim(d_clust$z)[2]
  cat("model-based optimal number of clusters:", m.best, "\n")
  # 4 clusters
  plot(d_clust)
  
  
  # 6.
  
  # Affinity propagation (AP) clustering, see http://dx.doi.org/10.1126/science.1136800
  
  library(apcluster)
  d.apclus <- apcluster(negDistMat(r=2), distMatrix)
  cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
  # 4
  heatmap(d.apclus)
  plot(d.apclus, d)
  
  return(x)
}
# -----------------------------------------------------------------------------------------------

PCA <- function(x)
{
# --------------Amit's Code---------Principal Component Analysis---------------------------------

  ingredientsDTM_Cuisine <- ingredientsDTM
  ingredientsDTM_Cuisine$cuisine <- as.factor(train$cuisine)
  
  # Partitioning the data with 75% in train
  inTrain3 <- createDataPartition(y = ingredientsDTM_Cuisine$cuisine, p = 0.75, list = FALSE)
  training3 <- ingredientsDTM_Cuisine[inTrain3,]
  pca.training3 <- subset(training3,select = -c(cuisine))# removing the cuisine(dependent) column
  testing3 <- ingredientsDTM_Cuisine[-inTrain3,]
  
  # Principle component Analysis
  prin_comp <- prcomp(pca.training3)
  names(prin_comp)
  dim(prin_comp$x)
  #standard deviation of each principal component
  std_dev <-prin_comp$sdev
  # Computing variance a higher variance implies more informaton is contained in that components.
  pr_var <- std_dev^2
  head(pr_var)
  
  # proportion of the variation
  prop_varex <- pr_var/sum(pr_var)
  head(prop_varex)
  #scree plot to understand the impact of principle componenets
  plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")
  # cumulative scree plot to understand the impact of principle componenets
  plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  
  return(x)
}