preserve_cl <- function (cl, cl_phi, k1, k2) {
  tab <- table(cl, cl_phi)
  k1_in <- (sum(tab[k1, ] != 0) == 1) & (sum(tab[, k1] != 0) ==
                                           1)
  k2_in <- (sum(tab[k2, ] != 0) == 1) & (sum(tab[, k2] != 0) ==
                                           1)
  k1_in & k2_in
}

harmonic_merge <- function(p){
  K <- length(p)
  return((exp(1)*log(K))*(K/(sum(1/p))))
}
