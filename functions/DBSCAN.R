############################ DBSCAN script for automated clustering ############################
dbscan_FCM <- function(dat) {
  if (!require(dbscan)) install.packages("dbscan")
  if (is.null(dat)) {
    print("No data provided. A dataframe is expected")
  } else {
    dbscan::hdbscan(dat, minPts = 10)
  }
}