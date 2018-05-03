#' GWLelast.cv.bw
#'
#' Cross validation for geographically weighted logistic elastic net regression
#'
#' @param x Covariates.
#' @param y Outcome binary variable.
#' @param coords 2 columns matrix including "longitude" and "latitude".
#' @param alpha The elasticnet mixing parameter [0,1] in glmnet package.
#' @param lambda Optional user-supplied lambda sequence in glmnet package.
#' @param nlambda The number of lambda values in glmnet package.
#' @param gweight geographical kernel function in spgwr package.
#' @param longlat Indicate if the coords parameter are sperically calculated.
#' @param bw bandwidth of geographical kernel function.
#' @param D Distance matrix.
#'
#' @return error Cross validation error.
#' @export

GWLelast.cv.bw = function(x = x, y = y, D = D, coords = coords, alpha = 1, lambda = lambda, nlambda = nlambda, gweight = gweight, longlat = longlat, bw = bw) {

  show(paste("Bandwidth: ", bw, "\n", sep = ""))

  GWLelast.model = GWLelast.est(x = x, y = y, coords= coords, alpha = alpha, lambda = lambda,
                                nlambda = nlambda, gweight = gweight,
                                longlat = longlat, bw = bw, D = D)

  error = sum(sapply(GWLelast.model[["error"]], sum))

  show(paste("Error of cross validation: ", error, "\n", sep = ""))

  return(error)
}
