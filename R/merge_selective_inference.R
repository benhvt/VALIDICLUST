#' Merged version of the selective test
#'
#' @param X The data matrix of size on which the clustering is applied
#' @param k1 The fisrt cluster of interest
#' @param k2 The second cluster of interest
#' @param g The variables for which the test is applied
#' @param ndraws The number of Monte-Carlo samples
#' @param cl_fun The clustering function used to build clusters
#' @param cl The labels of the data obtained thanks to the \code{cl_fun} function
#'
#' @import dplyr
#'
#' @return A list with the following elements \itemize{
#' \item \code{pval} : The resulting p-values of the test.
#' \item \code{adjacent} : List of the adjacent clusters between k1 and k2
#' \item \code{pval_adj} : The corresponding adjacent p-values that are merged
#' }
#' @export
#'
#' @examples
#' X <- matrix(rnorm(200),ncol = 2)
#' hcl_fun <- function(x){
#' return(as.factor(cutree(hclust(dist(x), method = "ward.D2"), k=4)))}
#' cl <- hcl_fun(X)
#'plot(X, col=cl)
#'test_var1 <- test_selective_inference(X, k1=1, k2=4, g=1, ndraws =2000, cl_fun = hcl_fun, cl = cl)

merge_selective_inference <- function(X, k1, k2, g, ndraws = 2000, cl_fun, cl){
  Xg <- data.frame(Xg=X[,g], Cluster = cl)
  Ck1k2 <- Xg %>% group_by(Cluster) %>% summarise(Mean = mean(Xg)) %>% arrange(Mean)
  adjacent <- as.numeric(Ck1k2$Cluster[which(Ck1k2$Cluster == k1):which(Ck1k2$Cluster == k2)])
  if (length(adjacent) == 2){
    pval_agg <- NA
    pval <- test_selective_inference(X,
                                         k1=k1,
                                         k2=k2,
                                         g=g,
                                         ndraws = ndraws,
                                         cl_fun = cl_fun,
                                         cl=cl)$pval
  }
  else{
    sig_g <- stats::sd(X[which(cl %in% adjacent), g])
    pval_agg <- rep(NA, (length(adjacent)-1))
    for (i in 1:(length(adjacent)-1)){
      pval_agg[i] <- test_selective_inference(X=X,
                                              k1=adjacent[i],
                                              k2=adjacent[i+1],
                                              g=g,
                                              ndraws = ndraws,
                                              cl_fun = cl_fun,
                                              cl=cl,
                                              sig = sig_g)$pval
    }
    pval <- min(harmonic_merge(pval_agg),1)
  }
  return(list(pval = pval,
              adjacent = adjacent,
              pval_adj = pval_agg))

}
