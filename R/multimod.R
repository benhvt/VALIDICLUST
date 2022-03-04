#' Multimodality test for post clustering variable involvement
#'
#' @param X The data matrix of size on which the clustering is applied
#' @param g The variable on which the test is applied
#' @param cl The labels of the data obtained thanks to a clusteirng algorithm
#' @param k1 The first cluster of interest
#' @param k2 The second cluster of interest
#'
#'@import diptest
#'@import dplyr
#'
#' @return A list containing :  A list with the following elements \itemize{
#' \item \code{data_for_test} : The data used for the test
#' \item \code{stat_g} : The dip statistic
#' \item \code{pval} : The resulting p-values of the test computed with the \code{diptest} function
#' }
#' @export
#'
#' @examples
#' X <- matrix(rnorm(200),ncol = 2)
#' hcl_fun <- function(x){
#' return(as.factor(cutree(hclust(dist(x), method = "ward.D2"), k=2)))}
#' cl <- hcl_fun(X)
#'plot(X, col=cl)
#'test_var1 <- test_multimod(X, g=1, k1=1, k2=2, cl = cl)
#'test_var2 <- test_multimod(X, g=2, k1=1, k2=2, cl = cl)

test_multimod <- function(X, g, cl, k1, k2){
  if (!is.matrix(X))
    stop("X should be a matrix")
  n <- nrow(X)
  q <- ncol(X)
  if ( g <1 | g> q)
    stop("g should be between 1 and q")
  K <- length(unique(cl))
  if (!(K <=(max(2,n))) & (K>=min(2,n)) & (K%%1 == 0))
    stop("number of clusters should be between 2 and n")
  if (!(k1 <=(max(1,K))) & (k1>=min(1,K)) & (k1%%1 == 0) |
      !(k2 <=(max(1,K))) & (k2>=min(1,K)) & (k2%%1 == 0))
    stop(paste("cluster labels should be between 1 and K",
               sep = ""))
  X.df <- data.frame(X=X[,g], Cluster = as.factor(cl))
  X.df$id <- 1:nrow(X.df)

  mean_in_cluster <- X.df %>% group_by(.data$Cluster) %>% summarise(mean_X = mean(.data$X))
  ord_X <- mean_in_cluster$Cluster[sort(mean_in_cluster$mean_X, index.return = T)$ix]

  clust_ret <- ord_X[which(ord_X==k1):which(ord_X==k2)]
  data <- X.df$X[which(X.df$Cluster %in% clust_ret)]
  dip <- dip.test(x=data)


  return(list(data_for_test = data, stat_g = as.numeric(dip$statistic), pval = dip$p.value))
}

