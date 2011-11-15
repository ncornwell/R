require("vars")
require("tawny")

btcdHedge <- function(p, start=1, interval=4)
{
  # Generate hedge using BTCD method, as defined by de Prado [2011].
  #
  # Args:
  #   p: matrix of instrument price data
  #   start: index into p, which to begin generating hedge
  #   interval: number of periods used to calibrate hedge
  #
  # Returns: BTCD hedge ratio vector

  end <- start+interval
  pvar <- VAR(p[start:end,], p=1, type="none")

  varfit <- fitted(pvar)
  B <- cov.shrink(p)        # shrink covariance
  A <- t(varfit) %*% varfit

  C <- chol(B)
  CInv <- solve(C)
  D <- t(CInv) %*% A %*% CInv
  eigens <- eigen(D)
  z <- eigens$vectors[,length(eigens$values)]
  x <- CInv %*% z
  hedge <- x/x[1]

  # perform sanity check
  tx <- t(x)
  num <- tx %*% A %*% x
  denom <- tx %*% B %*% x
  check <- num / denom
  if ((check - eigens$values[length(eigens$values)]) > 0.0001)
  {
    message("failed sanity check")
  }

  return (hedge)
}
