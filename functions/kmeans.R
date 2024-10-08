############################ K-means script for automated clustering ############################

kmeans_FCM <- function(dat) {
  if (!require(fpc)) install.packages("fpc")
  if (is.null(dat)) {
    print("No data provided. A dataframe is expected")
  } else {
    ch_score <- c()
    for (i in 2:10){
      km <- kmeans(sapply(dat,as.numeric,2), i)
      ch_score[i] <- calinhara(sapply(dat,as.numeric,2),
                              km$cluster,
                              cn = max(km$cluster))
    }
    k <- which(ch_score == max(na.omit(ch_score)))
    kmeans_clust <- kmeans(sapply(dat,as.numeric,2), centers = k)$cluster
    dat <- cbind(dat,kmeans_clust)
  }
  return(dat)
}