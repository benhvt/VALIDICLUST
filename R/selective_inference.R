#' Selective inference for post-clustering variable involvement
#'
#'
#' @param X The data matrix of size on which the clustering is applied
#' @param k1 The first cluster of interest
#' @param k2 The second cluster of interest
#' @param g The variables for which the test is applied
#' @param ndraws The number of Monte-Carlo samples
#' @param cl_fun The clustering function used to build clusters
#' @param cl The labels of the data obtained thanks to the \code{cl_fun} function
#' @param sig The estimated standard deviation. Default is NULL and the standard deviation is estimated using only observations in the two clusters of interest
#'
#'
#' @return A list with the following elements \itemize{
#' \item \code{stat_g} : the test statistic used for the test.
#' \item \code{pval} : The resulting p-values of the test.
#' \item \code{stder} : The standard deviation of the p-values computed thanks to the Monte-Carlo samples.
#' \item \code{clusters} : The labels of the data.
#' }
#' @export
#'@note This function is adapted from the clusterpval::test_clusters_approx() of Gao et al. (2022) (available on Github: https://github.com/lucylgao/clusterpval)
#'
#'@references
#'Gao, L. L., Bien, J., & Witten, D. (2022). Selective inference for hierarchical clustering. Journal of the American Statistical Association, (just-accepted), 1-27.
#'
#' @examples
#'X <- matrix(rnorm(200),ncol = 2)
#' hcl_fun <- function(x){
#' return(as.factor(cutree(hclust(dist(x), method = "ward.D2"), k=2)))}
#' cl <- hcl_fun(X)
#'plot(X, col=cl)
#'#Note that in practice the value of ndraws (the number of Monte-Carlo simulations must be higher)
#'test_var1 <- test_selective_inference(X, k1=1, k2=2, g=1, ndraws =100, cl_fun = hcl_fun, cl = cl)
#'
#'
#'


test_selective_inference <- function (X, k1, k2, g, ndraws = 2000, cl_fun, cl = NULL, sig = NULL)
{
  # This function is adapted from clusterpval::test_clusters_approx()
  if (!is.matrix(X))
    stop("X should be a matrix")
  n <- nrow(X)
  q <- ncol(X)
  if ( g <1 | g> q)
    stop("g should be between 1 and q")
  if (is.null(cl))
    cl <- cl_fun(X)
  K <- length(unique(cl))
  if (!(K <=(max(2,n))) & (K>=min(2,n)) & (K%%1 == 0))
    stop("number of clusters (K) should be between 2 and n")
  if (!(k1 <=(max(1,K))) & (k1>=min(1,K)) & (k1%%1 == 0) |
      !(k2 <=(max(1,K))) & (k2>=min(1,K)) & (k2%%1 == 0))
    stop(paste("cluster indices should be between 1 and K",
               sep = ""))
  n1 <- sum(cl == k1)
  n2 <- sum(cl == k2)
  squared_norm_nu <- 1/n1 + 1/n2
  stat_g <- mean(X[cl==k1, g]) - mean(X[cl == k2, g])
  if (is.null(sig)){
    sig <- stats::sd(X[cl%in%c(k1,k2),g])
  }
  scale_factor <- squared_norm_nu *sig^2
  log_survives <- rep(NA, ndraws)
  phi_g <- stats::rnorm(ndraws)*sqrt(scale_factor) + stat_g
  prop_k2 <- (n2)/(n1+n2)
  orig_k1 <- t(X[cl == k1, g])
  orig_k2 <- t(X[cl == k2, g])
  Xphi <- X
  for (j in 1:ndraws) {
    Xphi <- X
    Xphi[cl == k1, g] <- t(orig_k1 + (phi_g[j] - stat_g) * prop_k2)
    Xphi[cl == k2, g] <- t(orig_k2 + (phi_g[j] - stat_g) * (prop_k2-1))
    cl_Xphi <- cl_fun(Xphi)
    if (preserve_cl(cl, cl_Xphi, k1, k2)) {
      log_survives[j] <- stats::dnorm(phi_g[j], mean = 0, sd=sqrt(scale_factor), log=TRUE) -
        stats::dnorm(phi_g[j], mean = stat_g, sd = sqrt(scale_factor),
                     log = TRUE)
    }
  }
  phi_g <- phi_g[!is.na(log_survives)]

  log_survives <- log_survives[!is.na(log_survives)]
  survives <- length(log_survives)
  if (survives == 0) {
    warning("Oops - we didn't generate any samples that preserved the clusters! Try re-running with a larger value of ndraws.")
    return(list(stat = abs(stat_g), pval = NA, clusters = cl))
  }
  log_survives_shift <- log_survives - max(log_survives)
  cons <- mean(exp(log_survives))/max(exp(log_survives))
  pval <- (sum(exp(log_survives_shift)[abs(phi_g) > abs(stat_g)])+ cons)/(sum(exp(log_survives_shift))+cons)
  return(list(stat_g = abs(stat_g), pval = pval,
              clusters = cl))
}
