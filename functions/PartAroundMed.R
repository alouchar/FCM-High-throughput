############################ PAM script for automated clustering ############################

PAM_FCM <- function(dat){
  if (!require(cluster)) install.packages("cluster")
  if (is.null(dat)) {
    print("No data provided. A dataframe is expected")
  } else {
    ch_score <- c()
    for (i in 2:10){
      km <- pam(sapply(dat,as.numeric,2), i)
      ch_score[i] <- calinhara(sapply(dat,as.numeric,2),
                               km$cluster,
                               cn = max(km$cluster))
    }
    k <- which(ch_score == max(na.omit(ch_score)))
    PAM_clust <- pam(sapply(dat,as.numeric,2), k = k)$cluster
    dat <- cbind(dat,PAM_clust)
  }
  return(dat)
}
